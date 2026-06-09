//! Go component extraction (tree-sitter), matching legacy `parse_go`.
//!
//! Legacy semantics, reproduced deliberately:
//! - `type Name struct` / `type Name interface` headers (no generics: the
//!   legacy pattern required exactly `type \w+ (struct|interface)`);
//! - column-0 `func ...` signatures (declarations and methods), sliced up
//!   to the body brace, which must be surrounded by whitespace (the legacy
//!   lookahead was `(?=\s\{\s)`, so `func f() {}` never matched);
//! - signatures keep raw source text (tabs included; the legacy extractor
//!   did not strip comments for Go).

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::{parse, ExtractResult};

static TYPE_HEADER_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ *type \w+ (struct|interface)$").unwrap());

/// Extract Go components: type struct/interface headers and func signatures.
pub fn extract(content: &str) -> ExtractResult {
    let tree = parse(content, &tree_sitter_go::LANGUAGE.into())?;
    let mut components: Vec<String> = Vec::new();
    visit(tree.root_node(), content, &mut components);
    Ok(components)
}

fn line_start(content: &str, byte: usize) -> usize {
    content[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

fn visit(node: Node<'_>, content: &str, out: &mut Vec<String>) {
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        match child.kind() {
            "type_declaration" => {
                let mut tcursor = child.walk();
                for spec in child.named_children(&mut tcursor) {
                    if spec.kind() == "type_spec" {
                        emit_type_spec(spec, content, out);
                    }
                }
            }
            "function_declaration" | "method_declaration" => {
                emit_func(child, content, out);
                if let Some(body) = child.child_by_field_name("body") {
                    visit(body, content, out);
                }
            }
            _ => visit(child, content, out),
        }
    }
}

/// `type Name struct` / `type Name interface` when the body brace follows.
fn emit_type_spec(spec: Node<'_>, content: &str, out: &mut Vec<String>) {
    let Some(type_node) = spec.child_by_field_name("type") else {
        return;
    };
    let keyword_len = match type_node.kind() {
        "struct_type" => "struct".len(),
        "interface_type" => "interface".len(),
        _ => return,
    };
    // slice from the `type` keyword line through the struct/interface keyword
    let decl_start = spec
        .parent()
        .map(|p| p.start_byte())
        .unwrap_or_else(|| spec.start_byte());
    let start = line_start(content, decl_start);
    let end = type_node.start_byte() + keyword_len;
    let header = &content[start..end];
    if !TYPE_HEADER_RE.is_match(header) {
        return; // legacy pattern: single-space, no generics, no aliases
    }
    // legacy lookahead `(?=\s*\{)`
    if !content[end..].trim_start().starts_with('{') {
        return;
    }
    out.push(header.to_string());
}

/// Column-0 `func` signature up to (not including) the body brace.
fn emit_func(node: Node<'_>, content: &str, out: &mut Vec<String>) {
    let start = node.start_byte();
    // legacy `^func`: column 0 only
    if line_start(content, start) != start {
        return;
    }
    let Some(body) = node.child_by_field_name("body") else {
        return;
    };
    // legacy lookahead `(?=\s\{\s)`: whitespace on both sides of the brace
    let brace = body.start_byte();
    let before_ok = content[..brace]
        .chars()
        .next_back()
        .is_some_and(char::is_whitespace);
    let after_ok = content[brace + 1..]
        .chars()
        .next()
        .is_some_and(char::is_whitespace);
    if !before_ok || !after_ok {
        return;
    }
    let component = content[start..brace].trim_end();
    out.push(component.to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn types_and_funcs() {
        let content = "type Greeting struct {\n\tmessage string\n}\n\nfunc (g Greeting) sayHello() {\n\tfmt.Println(g.message)\n}\n\nfunc createGreeting(m string) Greeting {\n\treturn Greeting{message: m}\n}\n";
        assert_eq!(
            extract(content).unwrap(),
            vec![
                "type Greeting struct",
                "func (g Greeting) sayHello()",
                "func createGreeting(m string) Greeting",
            ]
        );
    }

    #[test]
    fn multiline_signature_keeps_tabs() {
        let content = "func WithAReasonableName(\n\tctx context.Context,\n\tparam1 string,\n) (resultType, error) {\n\treturn resultType{}, nil\n}\n";
        assert_eq!(
            extract(content).unwrap(),
            vec!["func WithAReasonableName(\n\tctx context.Context,\n\tparam1 string,\n) (resultType, error)"]
        );
    }

    #[test]
    fn legacy_quirks_preserved() {
        // empty body `{}`: the brace is not whitespace-surrounded -> skipped
        assert!(extract("func f() {}\n").unwrap().is_empty());
        // generic types don't match the legacy header pattern
        assert!(extract("type Box[T any] struct {\n\tv T\n}\n")
            .unwrap()
            .is_empty());
        // interfaces are headers too
        assert_eq!(
            extract("type Animal interface {\n\tSpeak() string\n}\n").unwrap(),
            vec!["type Animal interface"]
        );
    }
}
