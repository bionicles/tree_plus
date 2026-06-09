//! Kotlin component extraction, matching legacy `parse_kt`.
//!
//! This is a procedural port of the legacy combined regex rather than a
//! tree-sitter formatter: the community Kotlin grammar cannot recover from
//! the (deliberately invalid) constructs in the acceptance fixture — one
//! ERROR node swallows the rest of the file — while the legacy pattern is
//! line-anchored and keeps going. The scanner is a single forward pass.
//!
//! Legacy semantics, reproduced deliberately:
//! - type headers: `^ *(modifier )*(class|interface|object)` (no modifier
//!   may be `fun`), captured until a `{`, a blank line, or a column-0 word
//!   line, then right-trimmed — multi-line primary constructors survive;
//! - functions: `^.* ?fun (<generics> )?name(args)` where `args` stops at
//!   the first `)`, plus an optional `: CapitalizedType` return and an
//!   optional ` -> Word)` tail (which is how `(T) -> Unit)` parameters
//!   keep their closing paren), gated on a following whitespace;
//! - an indented ALL-CAPS `NAME {` line directly above a `fun` line is
//!   prepended to the component (enum entries with bodies);
//! - per component: strip comments, right-trim, replace `", \n"` with
//!   `",\n"`.

use std::sync::LazyLock;

use regex::Regex;

use super::treesitter::rust::strip_c_comments;

static TYPE_START_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ *((?:\w+ )*)(?:class|interface|object)[^.\n]").unwrap());

static ENUM_ENTRY_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ +[A-Z]+ \{").unwrap());

static RETURN_TYPE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\s?: [A-Z]\w*\??").unwrap());

static ARROW_TAIL_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^\s->\s\w+\)").unwrap());

/// Extract Kotlin components: classes/interfaces/objects and functions.
pub fn extract(content: &str) -> Vec<String> {
    let lines = line_offsets(content);
    let mut components = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        if let Some((component, next)) = match_type(content, &lines, i) {
            components.push(clean(&component));
            i = next;
            continue;
        }
        if let Some((component, next)) = match_fun(content, &lines, i) {
            components.push(clean(&component));
            i = next;
            continue;
        }
        i += 1;
    }
    components
}

/// (byte offset, line content without trailing newline) for each line.
fn line_offsets(content: &str) -> Vec<(usize, &str)> {
    let mut out = Vec::new();
    let mut offset = 0;
    for line in content.split('\n') {
        out.push((offset, line));
        offset += line.len() + 1;
    }
    out
}

fn clean(component: &str) -> String {
    strip_c_comments(component)
        .trim_end()
        .replace(", \n", ",\n")
}

/// Type headers; returns (component, next line index).
fn match_type(content: &str, lines: &[(usize, &str)], i: usize) -> Option<(String, usize)> {
    let (offset, line) = lines[i];
    let caps = TYPE_START_RE.captures(line)?;
    // no modifier word may be `fun` (legacy `(?<!fun )` lookbehind)
    if caps[1].split_whitespace().any(|w| w == "fun") {
        return None;
    }
    // capture until `{`, a blank line, or a column-0 word line; the raw
    // (untrimmed) tail matters: comment stripping runs before rstrip, and
    // a trailing space can keep `\s*\)` out of a line comment's reach
    if let Some(brace) = line.find('{') {
        return Some((line[..brace].to_string(), i + 1));
    }
    let mut end_line = i;
    for (j, &(_, next)) in lines.iter().enumerate().skip(i + 1) {
        if next.trim().is_empty() || next.starts_with(|c: char| c.is_alphanumeric() || c == '_') {
            break;
        }
        end_line = j;
        if next.contains('{') {
            break;
        }
    }
    let (last_offset, last_line) = lines[end_line];
    let cut = last_line.find('{').unwrap_or(last_line.len());
    let component = content[offset..last_offset + cut].to_string();
    Some((component, end_line + 1))
}

/// Functions; returns (component, next line index).
fn match_fun(content: &str, lines: &[(usize, &str)], i: usize) -> Option<(String, usize)> {
    // an enum-entry line directly above the fun line joins the component
    let (prefix, f) = if ENUM_ENTRY_RE.is_match(lines[i].1)
        && lines.get(i + 1).is_some_and(|l| l.1.contains("fun"))
    {
        (Some(lines[i].1), i + 1)
    } else {
        (None, i)
    };
    let (line_offset, line) = *lines.get(f)?;
    // legacy `^.* ?fun ` is greedy: try the last `fun ` first
    let mut search_end = line.len();
    while let Some(rel) = line[..search_end].rfind("fun ") {
        if let Some(end) = parse_fun_tail(content, line_offset, line, rel + 4) {
            let mut component = String::new();
            if let Some(p) = prefix {
                component.push_str(p);
                component.push('\n');
            }
            component.push_str(&content[line_offset..end]);
            // resume scanning on the line after the match end
            let end_line = lines.partition_point(|&(o, _)| o <= end) - 1;
            return Some((component, end_line + 1));
        }
        search_end = rel;
    }
    None
}

/// Validate `name(args)` + optional return/arrow tail starting at `name_rel`
/// (relative to `line`); returns the absolute end byte of the component.
fn parse_fun_tail(content: &str, line_offset: usize, line: &str, name_rel: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    let mut pos = name_rel;
    // optional `<generics> ` (balanced, same line)
    if bytes.get(pos) == Some(&b'<') {
        let close = balanced_angle(line, pos)?;
        if bytes.get(close + 1) != Some(&b' ') {
            return None;
        }
        pos = close + 2;
    }
    // name: words, dots, optional `?`, embedded `<generics>` chunks
    let name_start = pos;
    loop {
        match bytes.get(pos) {
            Some(c) if c.is_ascii_alphanumeric() || *c == b'_' || *c == b'.' => {
                pos += 1;
                if bytes.get(pos) == Some(&b'?') {
                    pos += 1;
                }
            }
            Some(&b'<') => pos = balanced_angle(line, pos)? + 1,
            _ => break,
        }
    }
    if pos == name_start || bytes.get(pos) != Some(&b'(') {
        return None;
    }
    // params: everything to the first `)` (may span lines)
    let abs_open = line_offset + pos;
    let close_rel = content[abs_open..].find(')')?;
    let mut end = abs_open + close_rel + 1;
    // optional `: CapitalizedType`, then optional ` -> Word)`
    if let Some(m) = RETURN_TYPE_RE.find(&content[end..]) {
        end += m.end();
    }
    if let Some(m) = ARROW_TAIL_RE.find(&content[end..]) {
        end += m.end();
    }
    // legacy `(?=\s)` gate
    content[end..]
        .chars()
        .next()
        .filter(|c| c.is_whitespace())
        .map(|_| end)
}

/// End index of the `>` balancing the `<` at `open` (same line only).
fn balanced_angle(line: &str, open: usize) -> Option<usize> {
    let mut depth = 0;
    for (k, c) in line.char_indices().skip(open) {
        match c {
            '<' => depth += 1,
            '>' => {
                depth -= 1;
                if depth == 0 {
                    return Some(k);
                }
            }
            _ => {}
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn data_class_and_fun() {
        let src =
            "data class Person(val name: String)\n\nfun greet(person: Person) = println(person)\n";
        assert_eq!(
            extract(src),
            vec![
                "data class Person(val name: String)",
                "fun greet(person: Person)"
            ]
        );
    }

    #[test]
    fn function_type_param_keeps_closing_paren() {
        let src = "fun <T> processItems(items: List<T>, processor: (T) -> Unit) {\n    items.forEach { processor(it) }\n}\n";
        assert_eq!(
            extract(src),
            vec!["fun <T> processItems(items: List<T>, processor: (T) -> Unit)"]
        );
    }

    #[test]
    fn multiline_class_with_comments() {
        let src = "class People(\n    firstNames: Array<String>, /* heard you like */\n    ages: Array<Int>(42), // edge cases galore\n) {\n    fun edgeCases(): Boolean {\n        return True\n    }\n}\n";
        assert_eq!(
            extract(src),
            vec![
                "class People(\n    firstNames: Array<String>,\n    ages: Array<Int>(42),\n)",
                "    fun edgeCases(): Boolean",
            ]
        );
    }

    #[test]
    fn enum_entry_prefix() {
        let src = " enum class E : Op {\n    PLUS {\n        override fun apply(t: Int): Int = t\n    };\n}\n";
        assert_eq!(
            extract(src),
            vec![
                " enum class E : Op",
                "    PLUS {\n        override fun apply(t: Int): Int",
            ]
        );
    }

    #[test]
    fn fun_modifier_blocks_type_match() {
        // `(?<!fun )`: a `fun interface` line is not a type header; the fun
        // pattern needs parens, so nothing is emitted for the header itself
        let src = "fun interface KRunnable {\n    fun invoke()\n}\n";
        assert_eq!(extract(src), vec!["    fun invoke()"]);
    }
}
