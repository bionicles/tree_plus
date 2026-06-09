//! Rust component extraction (tree-sitter), matching legacy `parse_rs`.
//!
//! Tree-sitter locates items (functions, structs, impls, enums, traits,
//! mods, macros); each item is then formatted by a direct port of the legacy
//! per-construct regex applied to the comment-stripped item slice, so the
//! emitted labels keep every legacy quirk (e.g. indented `impl` blocks are
//! skipped, enums require trailing commas on all variants, `pub(crate)`
//! visibility never matches).

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::{parse, ExtractResult};

/// Port of legacy `remove_c_comments` (note: `/* */` only on one line).
static C_COMMENT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)(\s)*//.*(\s*,\s*|\s*\))?$|\s*/\*.*?\*/").unwrap());

pub fn strip_c_comments(content: &str) -> String {
    C_COMMENT_RE.replace_all(content, "").into_owned()
}

/// Prefix of the legacy function pattern, up to the opening paren. The
/// argument scan (with its `\)(?!:)` lookahead) is emulated manually in
/// `format_fn` because the regex crate has no look-around.
static FN_PREFIX_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^\s*(pub\s+?)?((?:async|const)\s+)?fn\s+\w+(<[^>]*?>)?\(").unwrap()
});

static FN_END_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r";\s|\{").unwrap());

/// Legacy fn-arguments character set: `[&\w,.':\[\]()<>${}/\s]`.
fn fn_args_char_ok(c: char) -> bool {
    c.is_alphanumeric()
        || c == '_'
        || c.is_whitespace()
        || matches!(
            c,
            '&' | ','
                | '.'
                | '\''
                | ':'
                | '['
                | ']'
                | '('
                | ')'
                | '<'
                | '>'
                | '$'
                | '{'
                | '}'
                | '/'
        )
}

static STRUCT_IMPL_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\n(?P<struct_impl>(?: *((?:pub\s+)?struct)|impl)[^\{;]*?) ?[\{;]").unwrap()
});

static ENUM_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?m)^\s*(?P<enum>(?P<visibility>pub\s+?)?enum (?P<enum_name>\w+)(?P<maybe_generics><[^>]*?>)? \{(?P<variant>(?P<maybe_decorator>\s+#\[.*\]+?)?\s+(?P<variant_name>\w+)(?P<maybe_data>(?P<tuple_variant>\([\s\S]+?\))|(?P<struct_variant> \{[^}]+\}))?,)*\s\})\s",
    )
    .unwrap()
});

static TRAIT_MOD_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\n(?P<trait_mod> *(?:pub\s+)?(trait|mod)\s+\w*(<[^\{]*>)?)").unwrap()
});

static MACRO_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\n(?P<macro>(#\[macro_export\]\n)?macro_rules!\s+[a-z_][a-z_0-9]*)").unwrap()
});

/// Port of rich's `escape` (legacy applied it to enum components containing
/// attribute "decorators" when syntax highlighting was off).
static ESCAPE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(\\*)(\[[a-z#/@][^\[]*?\])").unwrap());

pub fn rich_escape(markup: &str) -> String {
    let escaped = ESCAPE_RE.replace_all(markup, |caps: &regex::Captures<'_>| {
        format!("{0}{0}\\{1}", &caps[1], &caps[2])
    });
    let escaped = escaped.into_owned();
    if escaped.ends_with('\\') && !escaped.ends_with("\\\\") {
        format!("{escaped}\\")
    } else {
        escaped
    }
}

/// Extract Rust components: fns, structs, impls, enums (with variants),
/// traits, mods, macro_rules.
pub fn extract(content: &str, syntax: bool) -> ExtractResult {
    let tree = parse(content, &tree_sitter_rust::LANGUAGE.into())?;
    let mut components: Vec<String> = Vec::new();
    visit(tree.root_node(), content, syntax, &mut components);
    Ok(components)
}

/// Depth-first via an explicit stack: AST depth is input-controlled, and
/// deep expression nesting must not overflow small worker-thread stacks.
fn visit(root: Node<'_>, content: &str, syntax: bool, out: &mut Vec<String>) {
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        match node.kind() {
            "function_item" | "function_signature_item" => {
                if let Some(c) = format_fn(node, content) {
                    out.push(c);
                }
                // nested items inside fn bodies still match
                descend(node, &mut stack);
            }
            "struct_item" => {
                if let Some(c) = format_struct_impl(node, content, false) {
                    out.push(c);
                }
            }
            "impl_item" => {
                if let Some(c) = format_struct_impl(node, content, true) {
                    out.push(c);
                }
                descend(node, &mut stack);
            }
            "enum_item" => {
                if let Some(c) = format_enum(node, content, syntax) {
                    out.push(c);
                }
            }
            "trait_item" | "mod_item" => {
                if let Some(c) = format_trait_mod(node, content) {
                    out.push(c);
                }
                descend(node, &mut stack);
            }
            "macro_definition" => {
                if let Some(c) = format_macro(node, content) {
                    out.push(c);
                }
            }
            _ => descend(node, &mut stack),
        }
    }
}

/// Push `node`'s named children so they pop in source order.
fn descend<'t>(node: Node<'t>, stack: &mut Vec<Node<'t>>) {
    let mut cursor = node.walk();
    let children: Vec<Node<'t>> = node.named_children(&mut cursor).collect();
    stack.extend(children.into_iter().rev());
}

/// Start of the line containing `byte` (offset just after the previous `\n`).
fn line_start(content: &str, byte: usize) -> usize {
    content[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

fn format_fn(node: Node<'_>, content: &str) -> Option<String> {
    node.child_by_field_name("parameters")?;
    let start = line_start(content, node.start_byte());
    // include one byte past the node so `;\s` can see the newline
    let end = (node.end_byte() + 1).min(content.len());
    let mut slice = strip_c_comments(&content[start..end]);
    if !slice.ends_with(['\n', ' ', '{']) {
        slice.push('\n'); // give `;\s` a whitespace to match at EOF
    }
    let open = FN_PREFIX_RE.find(&slice)?.end(); // offset just past `(`
                                                 // emulate `(?P<arguments>charset+?)?\)(?!:)`: accept the first `)` whose
                                                 // preceding argument text fits the charset and which `:` does not follow
    let chars: Vec<(usize, char)> = slice[open..].char_indices().collect();
    let mut args_ok = true;
    let mut close: Option<usize> = None; // byte offset in `slice` of `)`
    for (i, (off, c)) in chars.iter().enumerate() {
        if *c == ')' {
            let next = chars.get(i + 1).map(|(_, n)| *n);
            if next != Some(':') {
                close = Some(open + off);
                break;
            }
            // `)` followed by `:` -> keep scanning, `)` stays in arguments
        }
        if !fn_args_char_ok(*c) {
            args_ok = false;
            break;
        }
    }
    if !args_ok {
        return None;
    }
    let close = close?;
    let end_match = FN_END_RE.find(&slice[close + 1..])?;
    let component = &slice[..close + 1 + end_match.start()];
    let component = component
        .trim_end()
        .trim_end_matches(',')
        .trim_end_matches('\n')
        .trim_end_matches(';');
    Some(component.trim_start_matches('\n').to_string())
}

fn format_struct_impl(node: Node<'_>, content: &str, is_impl: bool) -> Option<String> {
    let start = line_start(content, node.start_byte());
    if start == 0 {
        return None; // legacy pattern required a preceding newline
    }
    if is_impl && node.start_byte() != start {
        return None; // legacy: impl only matches at column 0
    }
    let end = node.end_byte().min(content.len());
    // prepend the newline the legacy pattern consumed
    let slice = format!("\n{}", strip_c_comments(&content[start..end]));
    let caps = STRUCT_IMPL_RE.captures(&slice)?;
    if caps.get(0)?.start() != 0 {
        return None;
    }
    let component = caps.name("struct_impl")?.as_str().trim_end();
    Some(component.trim_start_matches('\n').to_string())
}

fn format_enum(node: Node<'_>, content: &str, syntax: bool) -> Option<String> {
    // attributes (e.g. #[derive]) come before the enum keyword inside the
    // node? No: attribute_item is a sibling. The enum slice starts at the
    // `pub`/`enum` keyword.
    let start = line_start(content, node.start_byte());
    let end = (node.end_byte() + 1).min(content.len());
    let mut slice = strip_c_comments(&content[start..end]);
    if !slice.ends_with(char::is_whitespace) {
        slice.push('\n'); // trailing `\s` in the legacy pattern
    }
    let caps = ENUM_RE.captures(&slice)?;
    if caps.get(0)?.start() != 0 {
        return None;
    }
    let component = caps.name("enum")?.as_str().trim_start_matches('\n');
    let has_decorator = caps.name("maybe_decorator").is_some();
    if !syntax && has_decorator {
        Some(rich_escape(component))
    } else {
        Some(component.to_string())
    }
}

fn format_trait_mod(node: Node<'_>, content: &str) -> Option<String> {
    let start = line_start(content, node.start_byte());
    if start == 0 {
        return None;
    }
    let end = node.end_byte().min(content.len());
    let slice = format!("\n{}", strip_c_comments(&content[start..end]));
    let caps = TRAIT_MOD_RE.captures(&slice)?;
    if caps.get(0)?.start() != 0 {
        return None;
    }
    Some(
        caps.name("trait_mod")?
            .as_str()
            .trim_start_matches('\n')
            .to_string(),
    )
}

fn format_macro(node: Node<'_>, content: &str) -> Option<String> {
    // include a possible #[macro_export] attribute line directly above
    let mut start = line_start(content, node.start_byte());
    let prefix = "#[macro_export]\n";
    if start >= prefix.len() && content[..start].ends_with(prefix) {
        start -= prefix.len();
    }
    if start == 0 {
        return None;
    }
    let end = node.end_byte().min(content.len());
    let slice = format!("\n{}", strip_c_comments(&content[start..end]));
    let caps = MACRO_RE.captures(&slice)?;
    if caps.get(0)?.start() != 0 {
        return None;
    }
    Some(
        caps.name("macro")?
            .as_str()
            .trim_start_matches('\n')
            .to_string(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_fn() {
        let content = "fn add(x1: i64, x2: i64) -> i64 {\n    x1 + x2\n}\n";
        assert_eq!(
            extract(content, false).unwrap(),
            vec!["fn add(x1: i64, x2: i64) -> i64"]
        );
    }

    #[test]
    fn impl_and_method() {
        let content = "struct Point;\nimpl Point {\n    fn get_origin() -> Point {\n        Point\n    }\n}\n";
        assert_eq!(
            extract(content, false).unwrap(),
            vec!["impl Point", "    fn get_origin() -> Point"]
        );
    }

    #[test]
    fn escape_matches_rich() {
        assert_eq!(rich_escape("#[default]"), "#\\[default]");
        assert_eq!(rich_escape("[Topic]"), "[Topic]"); // uppercase: untouched
    }
}
