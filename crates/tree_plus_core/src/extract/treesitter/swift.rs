//! Swift component extraction (tree-sitter), matching legacy `parse_swift`.
//!
//! Legacy semantics, reproduced deliberately:
//! - type headers: `^(class|struct|protocol|enum).*` up to a ` {` on the
//!   same line — column 0 only, keyword first (so `public class` and
//!   indented/nested types were never matched);
//! - functions and initializers: `^ *(func|init)\s*\w*\s*\(args\)` with an
//!   optional `-> \w+` return, gated on a following ` {`, `;`, or newline;
//!   no modifiers allowed (`override func` was never matched);
//! - matching ran on comment-stripped text, so comments vanish from
//!   multi-line signatures but raw spacing/newlines are kept.

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::rust::strip_c_comments;
use super::{parse, ExtractResult};

static TYPE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:class|struct|protocol|enum)").unwrap());

static FUNC_START_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^( *)(func|init)\s*\w*\s*\(").unwrap());

static RETURN_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^\s*->\s*\w+").unwrap());

/// Extract Swift components: type headers, funcs, inits.
pub fn extract(content: &str) -> ExtractResult {
    let tree = parse(content, &tree_sitter_swift::LANGUAGE.into())?;
    let mut extractor = SwiftExtractor {
        content,
        components: Vec::new(),
    };
    extractor.run(tree.root_node());
    Ok(extractor.components)
}

struct SwiftExtractor<'a> {
    content: &'a str,
    components: Vec<String>,
}

fn line_start(content: &str, byte: usize) -> usize {
    content[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

impl<'a> SwiftExtractor<'a> {
    /// Depth-first via an explicit stack: AST depth is input-controlled, and
    /// extraction runs on small worker-thread stacks.
    fn run(&mut self, root: Node<'a>) {
        let mut stack = vec![root];
        while let Some(node) = stack.pop() {
            match node.kind() {
                "class_declaration" | "protocol_declaration" => {
                    // covers class/struct/enum (one node kind) and protocol
                    self.emit_type_header(node);
                    if let Some(body) = node.child_by_field_name("body") {
                        stack.push(body);
                    }
                }
                "function_declaration" | "protocol_function_declaration" | "init_declaration" => {
                    self.emit_function(node);
                    if let Some(body) = node.child_by_field_name("body") {
                        stack.push(body);
                    }
                }
                _ => {
                    let mut cursor = node.walk();
                    let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();
                    stack.extend(children.into_iter().rev());
                }
            }
        }
    }

    /// `class Person`, `struct Dog: Animal`, ... — column-0 keyword line,
    /// captured up to the last ` {` on that line.
    fn emit_type_header(&mut self, node: Node<'a>) {
        let start = node.start_byte();
        if start != line_start(self.content, start) {
            return; // legacy pattern had no indent allowance
        }
        let line = self.content[start..].split('\n').next().unwrap_or("");
        let line = strip_c_comments(line);
        if !TYPE_RE.is_match(&line) {
            return;
        }
        let Some(brace) = line.rfind(" {") else {
            return; // `{` must follow on the same line after a space
        };
        self.components.push(line[..brace].to_string());
    }

    /// ` *(func|init) name(args) -> T` gated on ` {`, `;`, or newline.
    fn emit_function(&mut self, node: Node<'a>) {
        let start = line_start(self.content, node.start_byte());
        let region = &self.content[start..];
        let Some(caps) = FUNC_START_RE.captures(region) else {
            return; // modifiers (e.g. `override func`) never matched
        };
        let open = caps.get(0).unwrap().end() - 1; // the `(`
        let Some(close_rel) = region[open..].find(')') else {
            return;
        };
        let mut end = open + close_rel + 1;
        // optional single-word return type
        if let Some(m) = RETURN_RE.find(&region[end..]) {
            end += m.end();
        }
        // legacy lookahead: ` {`, `;`, or newline must follow
        let after = &region[end..];
        if !(after.starts_with(" {") || after.starts_with(';') || after.starts_with('\n')) {
            // tolerate extra whitespace before the brace like `\s*` did
            let trimmed = after.trim_start_matches(' ');
            if !(trimmed.starts_with('{') && after.starts_with(' ')) {
                return;
            }
        }
        let component = strip_c_comments(&region[..end]);
        self.components.push(component.trim_end().to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn types_and_functions() {
        let src = "class Person {\n    init(name: String) {\n        self.name = name\n    }\n    func greet() {\n        print(\"hi\")\n    }\n}\n\nfunc top() -> Int {\n    return 1\n}\n";
        let got = extract(src).unwrap();
        assert_eq!(
            got,
            vec![
                "class Person",
                "    init(name: String)",
                "    func greet()",
                "func top() -> Int",
            ]
        );
    }

    #[test]
    fn indented_types_and_modified_funcs_skipped() {
        let src =
            "public class A {\n    override func f() {\n        g()\n    }\n}\nstruct B: P {\n}\n";
        let got = extract(src).unwrap();
        // `public class` not keyword-first; `override func` has a modifier;
        // struct B's body brace qualifies
        assert_eq!(got, vec!["struct B: P"]);
    }
}
