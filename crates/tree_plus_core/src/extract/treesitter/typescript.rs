//! JavaScript/TypeScript component extraction (tree-sitter).
//!
//! Emits source-text signature slices in legacy `parse_ts` style:
//! - classes/interfaces with their heritage clauses;
//! - methods and constructors (signature only, original indentation);
//! - function declarations and expressions (signature only);
//! - arrow functions bound to variables or object members, ending at `=>`;
//! - `const name = {` "object scope" lines when the object holds functions;
//! - `type Name` aliases.
//!
//! Known intentional differences from the legacy regex (documented in
//! docs/rust-port-differences.md): regex noise like bare call statements
//! (`super(...)`, `innerFunction("inner")`) and string literals containing
//! the word "function" are no longer emitted.

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::rust::strip_c_comments;
use super::{parse, ExtractResult};

/// Legacy `parse_ts` function preamble: what may precede the `function`
/// keyword on its line for the match to fire.
static FN_PREAMBLE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"^ *(export (default )?)?(\(|(const|var|let) \w+ = |return |\w+: )?(async )? ?(\w+\()?$",
    )
    .unwrap()
});

/// Legacy jsdoc line patterns (applied to a `/** ... */` comment preceding a
/// component; tagged lines keep matching, untagged only before any tag).
static JSDOC_TAGGED_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?m)^ +\* +@(category|typedefn|sig|param|returns?)( \{\{?[\s\S]*?\}?\})?.*")
        .unwrap()
});
static JSDOC_UNTAGGED_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^ \* \w+.*").unwrap());

/// Extract JS/TS components.
pub fn extract(content: &str, tsx: bool) -> ExtractResult {
    let language = if tsx {
        tree_sitter_typescript::LANGUAGE_TSX
    } else {
        tree_sitter_typescript::LANGUAGE_TYPESCRIPT
    };
    let tree = parse(content, &language.into())?;
    let mut extractor = TsExtractor {
        content,
        components: Vec::new(),
    };
    extractor.visit(tree.root_node());
    Ok(extractor.components)
}

struct TsExtractor<'a> {
    content: &'a str,
    components: Vec<String>,
}

fn line_start(content: &str, byte: usize) -> usize {
    content[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

impl<'a> TsExtractor<'a> {
    /// Legacy jsdoc carry: a `/** ... */` comment directly above a component
    /// contributes its tagged (and leading untagged) lines.
    fn jsdoc_prefix(&self, slice_start: usize) -> Option<String> {
        let before = self.content[..slice_start].trim_end();
        if !before.ends_with("*/") {
            return None;
        }
        let open = before.rfind("/**")?;
        let comment = &before[open..];
        let mut lines: Vec<&str> = Vec::new();
        let mut seen_tagged = false;
        let mut pos = 0;
        while pos < comment.len() {
            let tagged = JSDOC_TAGGED_RE.find_at(comment, pos);
            let untagged = JSDOC_UNTAGGED_RE.find_at(comment, pos);
            let pick = match (tagged, untagged) {
                (Some(t), Some(u)) => {
                    if t.start() <= u.start() {
                        (t, true)
                    } else {
                        (u, false)
                    }
                }
                (Some(t), None) => (t, true),
                (None, Some(u)) => (u, false),
                (None, None) => break,
            };
            let (m, is_tagged) = pick;
            if is_tagged {
                seen_tagged = true;
                lines.push(m.as_str());
            } else if !seen_tagged {
                lines.push(m.as_str());
            }
            pos = m.end().max(pos + 1);
        }
        if lines.is_empty() {
            return None;
        }
        Some(format!("/**\n{}\n */\n", lines.join("\n")))
    }

    fn push_with_jsdoc(&mut self, slice_start: usize, component: String) {
        match self.jsdoc_prefix(slice_start) {
            Some(prefix) => self.components.push(format!("{prefix}{component}")),
            None => self.components.push(component),
        }
    }

    fn slice(&self, from: usize, to: usize) -> String {
        // legacy parse_ts ran on comment-stripped content
        strip_c_comments(&self.content[from..to])
            .trim_end()
            .to_string()
    }

    fn visit(&mut self, node: Node<'a>) {
        match node.kind() {
            "class_declaration" | "abstract_class_declaration" => {
                self.emit_class_like(node);
                if let Some(body) = node.child_by_field_name("body") {
                    self.visit(body);
                }
            }
            "interface_declaration" => {
                self.emit_class_like(node);
            }
            "type_alias_declaration" => {
                if let Some(name) = node.child_by_field_name("name") {
                    let mut label = format!("type {}", &self.content[name.byte_range()]);
                    if let Some(tp) = node.child_by_field_name("type_parameters") {
                        label.push_str(&self.content[tp.byte_range()]);
                    }
                    self.components.push(label);
                }
            }
            "function_declaration" | "generator_function_declaration" => {
                self.emit_function(node);
                if let Some(body) = node.child_by_field_name("body") {
                    self.visit(body);
                }
            }
            "function_expression" | "generator_function" => {
                if self.function_expression_context(node) {
                    self.emit_function(node);
                }
                if let Some(body) = node.child_by_field_name("body") {
                    self.visit(body);
                }
            }
            "method_definition" | "method_signature" | "abstract_method_signature" => {
                self.emit_signature_to_params_or_return(node);
                if let Some(body) = node.child_by_field_name("body") {
                    self.visit(body);
                }
            }
            "arrow_function" => {
                if self.arrow_context(node) {
                    self.emit_arrow(node);
                }
                if let Some(body) = node.child_by_field_name("body") {
                    self.visit(body);
                }
            }
            "expression_statement" => {
                // legacy "method" noise: indented bare calls like `super(x)`
                if let Some(call) = node
                    .named_child(0)
                    .filter(|c| matches!(c.kind(), "call_expression"))
                {
                    self.maybe_emit_bare_call(node, call);
                }
                let mut cursor = node.walk();
                for child in node.named_children(&mut cursor) {
                    self.visit(child);
                }
            }
            "variable_declarator" => {
                // object scope: `const name = {` when the object holds functions
                if let Some(value) = node.child_by_field_name("value") {
                    if value.kind() == "object" && object_holds_functions(value) {
                        let start = line_start(self.content, node.start_byte());
                        // slice through the object's opening brace
                        let brace = value.start_byte();
                        self.components
                            .push(self.content[start..=brace].trim_end().to_string());
                    }
                    self.visit(value);
                }
            }
            _ => {
                let mut cursor = node.walk();
                for child in node.named_children(&mut cursor) {
                    self.visit(child);
                }
            }
        }
    }

    /// class/interface: line start through heritage (extends/implements).
    fn emit_class_like(&mut self, node: Node<'a>) {
        let Some(name) = node.child_by_field_name("name") else {
            return;
        };
        let mut end = name.end_byte();
        if let Some(tp) = node.child_by_field_name("type_parameters") {
            end = end.max(tp.end_byte());
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if matches!(
                child.kind(),
                "class_heritage" | "extends_clause" | "implements_clause" | "extends_type_clause"
            ) {
                end = end.max(child.end_byte());
            }
        }
        let start = line_start(self.content, outer_start(node));
        self.components.push(self.slice(start, end));
    }

    /// Functions: line start through return type or params; legacy stripped
    /// leading `(` from IIFE matches.
    fn emit_function(&mut self, node: Node<'a>) {
        let Some(params) = node.child_by_field_name("parameters") else {
            return;
        };
        let end = node
            .child_by_field_name("return_type")
            .map(|r| {
                // legacy return-type capture was lazy and stopped before " {"
                let text = &self.content[r.byte_range()];
                match text.find(" {") {
                    Some(i) => r.start_byte() + i,
                    None => r.end_byte(),
                }
            })
            .unwrap_or_else(|| params.end_byte());
        let start = line_start(self.content, outer_start(node));
        let text = self.slice(start, end);
        self.push_with_jsdoc(start, text.trim_start_matches('(').to_string());
    }

    /// Methods/constructors: indentation + signature through return type.
    fn emit_signature_to_params_or_return(&mut self, node: Node<'a>) {
        let Some(params) = node.child_by_field_name("parameters") else {
            return;
        };
        let end = node
            .child_by_field_name("return_type")
            .map(|r| r.end_byte())
            .unwrap_or_else(|| params.end_byte());
        let start = line_start(self.content, node.start_byte());
        self.components.push(self.slice(start, end));
    }

    /// Legacy method-pattern noise: an indented statement `name(args);` (or
    /// `super(args);`) matched the method signature regex and was emitted.
    fn maybe_emit_bare_call(&mut self, stmt: Node<'a>, call: Node<'a>) {
        let Some(callee) = call.child_by_field_name("function") else {
            return;
        };
        if !matches!(callee.kind(), "identifier" | "super") {
            return;
        }
        let Some(args) = call.child_by_field_name("arguments") else {
            return;
        };
        // `\w+\(...` -- no gap between callee and args, single-line args
        if args.start_byte() != callee.end_byte() {
            return;
        }
        if self.content[args.byte_range()].contains('\n') {
            return;
        }
        // must start an indented line
        let start = line_start(self.content, stmt.start_byte());
        let indent = &self.content[start..callee.start_byte()];
        if indent.is_empty() || !indent.chars().all(|c| c == ' ' || c == '\t') {
            return;
        }
        // fin: `;` or ` {` after the call (legacy pattern requirement)
        let after = &self.content[call.end_byte()..];
        if !(after.starts_with(';') || after.starts_with(" {")) {
            return;
        }
        self.components.push(self.slice(start, call.end_byte()));
    }

    /// Arrows: line start through the `=>` token.
    fn emit_arrow(&mut self, node: Node<'a>) {
        let mut arrow_end = None;
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "=>" {
                arrow_end = Some(child.end_byte());
                break;
            }
        }
        let Some(end) = arrow_end else { return };
        let start = line_start(self.content, outer_start(node));
        let text = self.slice(start, end);
        self.push_with_jsdoc(start, text);
    }

    /// Legacy gate for function expressions: the text before the `function`
    /// keyword on its line had to match the legacy preamble pattern
    /// (optional export/assignment/return/key, optional `name(` wrapper).
    fn function_expression_context(&self, node: Node<'a>) -> bool {
        let fn_kw = node.start_byte();
        let start = line_start(self.content, fn_kw);
        let prefix = &self.content[start..fn_kw];
        FN_PREAMBLE_RE.is_match(prefix)
    }

    fn arrow_context(&self, node: Node<'a>) -> bool {
        let Some(parent) = node.parent() else {
            return false;
        };
        matches!(
            parent.kind(),
            "variable_declarator" | "pair" | "assignment_expression"
        )
    }
}

/// Start byte including an `export ...` wrapper, but excluding decorators
/// (the legacy extractor never captured them).
fn outer_start(node: Node<'_>) -> usize {
    // skip the node's own leading decorators
    let mut start = node.start_byte();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() != "decorator" {
            start = child.start_byte();
            break;
        }
    }
    if let Some(parent) = node.parent() {
        if parent.kind() == "export_statement" {
            // anchor on the `export` keyword itself, past any decorators
            let mut pcursor = parent.walk();
            for child in parent.children(&mut pcursor) {
                if child.kind() == "export" {
                    return child.start_byte();
                }
            }
        }
    }
    start
}

/// Whether an object literal directly contains function/arrow members.
fn object_holds_functions(object: Node<'_>) -> bool {
    let mut cursor = object.walk();
    for pair in object.named_children(&mut cursor) {
        if pair.kind() == "pair" {
            if let Some(value) = pair.child_by_field_name("value") {
                if matches!(
                    value.kind(),
                    "function_expression" | "arrow_function" | "generator_function"
                ) {
                    return true;
                }
            }
        }
        if pair.kind() == "method_definition" {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn class_with_methods() {
        let content = "export class AppComponent {\n  constructor(private http: HttpClient) {}\n  checkSession() {}\n  async goToEvent(event_id: string) {}\n}\n";
        assert_eq!(
            extract(content, false).unwrap(),
            vec![
                "export class AppComponent",
                "  constructor(private http: HttpClient)",
                "  checkSession()",
                "  async goToEvent(event_id: string)",
            ]
        );
    }

    #[test]
    fn arrows_and_functions() {
        let content = "const arrow = (a: String, b: Number) => {\n  return a;\n};\nfunction tsFunction() {\n}\n";
        assert_eq!(
            extract(content, false).unwrap(),
            vec![
                "const arrow = (a: String, b: Number) =>",
                "function tsFunction()"
            ]
        );
    }

    #[test]
    fn object_scope() {
        let content =
            "const myObject = {\n  myMethod: function (stuff) {\n    return stuff;\n  },\n};\n";
        assert_eq!(
            extract(content, false).unwrap(),
            vec!["const myObject = {", "  myMethod: function (stuff)"]
        );
    }
}
