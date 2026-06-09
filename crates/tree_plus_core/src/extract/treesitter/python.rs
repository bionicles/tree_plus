//! Python component extraction (tree-sitter), matching legacy `parse_py`.
//!
//! The legacy extractor was a single multi-pattern regex over
//! comment-and-docstring-stripped source. The formatter here reproduces its
//! observable quirks deliberately, e.g.:
//! - `async def` is never emitted (the legacy pattern required `^ *def`);
//! - decorators are carried until the next matching def/class and dropped
//!   only when consumed;
//! - ALL-CAPS assignments ("enum variants") match anywhere, with int, double
//!   quoted string, or multiline constructor values;
//! - annotated fields are emitted only while the most recent component was a
//!   class ("class context"), which persists past the class body.

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::{node_text, parse, ExtractResult};

static COMMENT_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\s*#.*\n").unwrap());
static DECORATOR_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^@\w+(\(.*\))?$").unwrap());
static RETURN_TYPE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"^[\w"'\[\],. ]+$"#).unwrap());
static SUPERCLASSES_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\([\w\[\]\s,=.]*\)$").unwrap());
static VERSION_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"^__version__ = ".*""#).unwrap());
static TYPEVAR_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\w+ = TypeVar\([^)]+\)").unwrap());
static INT_VALUE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^\d+").unwrap());
static STR_VALUE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"^"+[^"]+"+"#).unwrap());
static STRUCT_CLOSING_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?m)^\s*\)$").unwrap());
static FIELD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\s+\w+:\s+[\w\[\]|., ]+").unwrap());
static ALLCAPS_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[A-Z_\d]+$").unwrap());

/// Remove `# ...` comments the way the legacy extractor did.
fn strip_comments(slice: &str) -> String {
    COMMENT_RE.replace_all(slice, "\n").into_owned()
}

struct PyExtractor<'a> {
    content: &'a str,
    components: Vec<String>,
    pending_decorators: Vec<String>,
    in_class: bool,
}

/// Extract Python components: defs, classes, decorators, TypeVars,
/// `__version__`, enum variants, dataclass fields.
pub fn extract(content: &str) -> ExtractResult {
    let tree = parse(content, &tree_sitter_python::LANGUAGE.into())?;
    let mut extractor = PyExtractor {
        content,
        components: Vec::new(),
        pending_decorators: Vec::new(),
        in_class: false,
    };
    extractor.run(tree.root_node());
    Ok(extractor.components)
}

impl<'a> PyExtractor<'a> {
    /// Byte offset of the start of the run of spaces before `start`
    /// (legacy patterns matched ` *` indentation, spaces only).
    fn space_indent_start(&self, start: usize) -> usize {
        let bytes = self.content.as_bytes();
        let mut i = start;
        while i > 0 && bytes[i - 1] == b' ' {
            i -= 1;
        }
        i
    }

    /// Byte offset of the start of `[ \t]*` indentation before `start`.
    fn ws_indent_start(&self, start: usize) -> usize {
        let bytes = self.content.as_bytes();
        let mut i = start;
        while i > 0 && (bytes[i - 1] == b' ' || bytes[i - 1] == b'\t') {
            i -= 1;
        }
        i
    }

    fn is_line_start(&self, offset: usize) -> bool {
        offset == 0 || self.content.as_bytes()[offset - 1] == b'\n'
    }

    fn emit_with_decorators(&mut self, component: String) {
        if self.pending_decorators.is_empty() {
            self.components.push(component);
        } else {
            let mut joined = self.pending_decorators.join("\n");
            joined.push('\n');
            joined.push_str(&component);
            self.pending_decorators.clear();
            self.components.push(joined);
        }
    }

    /// Depth-first via an explicit stack: AST depth is input-controlled
    /// (deeply nested expressions), and extraction runs on small
    /// worker-thread stacks.
    fn run(&mut self, root: Node<'a>) {
        let mut stack = vec![root];
        while let Some(node) = stack.pop() {
            match node.kind() {
                "decorated_definition" => {
                    // decorators collect now; the definition pops next
                    let mut cursor = node.walk();
                    let mut deferred: Vec<Node<'a>> = Vec::new();
                    for child in node.named_children(&mut cursor) {
                        if child.kind() == "decorator" {
                            self.collect_decorator(child);
                        } else {
                            deferred.push(child);
                        }
                    }
                    stack.extend(deferred.into_iter().rev());
                }
                "function_definition" => self.handle_function(node, &mut stack),
                "class_definition" => self.handle_class(node, &mut stack),
                "expression_statement" => {
                    let mut cursor = node.walk();
                    for child in node.named_children(&mut cursor) {
                        if child.kind() == "assignment" {
                            self.handle_assignment(child);
                        }
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

    fn collect_decorator(&mut self, node: Node<'a>) {
        // legacy: ` *@\w+(\(.*\))?\n` -- single line, simple name, no dots
        let text = node_text(node, self.content);
        if node.start_position().row != node.end_position().row {
            return;
        }
        // the decorator must end its line (only whitespace/comment was removed)
        let after = &self.content[node.end_byte()..];
        let line_rest = after.split('\n').next().unwrap_or("");
        if !line_rest.trim_start_matches([' ', '\t']).is_empty()
            && !line_rest.trim_start().starts_with('#')
        {
            return;
        }
        if !DECORATOR_RE.is_match(text) {
            return;
        }
        let indent_start = self.space_indent_start(node.start_byte());
        self.pending_decorators
            .push(self.content[indent_start..node.end_byte()].to_string());
    }

    fn handle_function(&mut self, node: Node<'a>, stack: &mut Vec<Node<'a>>) {
        let body = node.child_by_field_name("body");
        let is_async = node.child(0).map(|c| c.kind() == "async").unwrap_or(false);
        if !is_async {
            if let Some(component) = self.build_function_signature(node) {
                self.emit_with_decorators(component);
                self.in_class = false;
            }
        }
        if let Some(body) = body {
            stack.push(body);
        }
    }

    fn build_function_signature(&self, node: Node<'a>) -> Option<String> {
        let params = node.child_by_field_name("parameters")?;
        // legacy params pattern allowed nesting depth <= 2 inside the parens
        let params_text = node_text(params, self.content);
        let mut depth: i32 = 0;
        let mut max_inner: i32 = 0;
        for ch in params_text.chars() {
            match ch {
                '(' => {
                    depth += 1;
                    max_inner = max_inner.max(depth - 1);
                }
                ')' => depth -= 1,
                _ => {}
            }
        }
        if max_inner > 2 {
            return None;
        }
        let end = match node.child_by_field_name("return_type") {
            Some(ret) => {
                let ret_text = node_text(ret, self.content);
                if !RETURN_TYPE_RE.is_match(&strip_comments(ret_text)) {
                    return None; // legacy pattern would fail entirely
                }
                ret.end_byte()
            }
            None => params.end_byte(),
        };
        // find the `def` keyword start (node start may equal it)
        let def_kw = node
            .children(&mut node.walk())
            .find(|c| c.kind() == "def")
            .map(|c| c.start_byte())
            .unwrap_or(node.start_byte());
        let start = self.space_indent_start(def_kw);
        let slice = &self.content[start..end];
        let cleaned = strip_comments(slice);
        Some(cleaned)
    }

    fn handle_class(&mut self, node: Node<'a>, stack: &mut Vec<Node<'a>>) {
        let body = node.child_by_field_name("body");
        if let Some(component) = self.build_class_signature(node) {
            self.emit_with_decorators(component);
            self.in_class = true;
        }
        if let Some(body) = body {
            stack.push(body);
        }
    }

    fn build_class_signature(&self, node: Node<'a>) -> Option<String> {
        let name = node.child_by_field_name("name")?;
        let mut end = name.end_byte();
        if let Some(tp) = node.child_by_field_name("type_parameters") {
            end = tp.end_byte();
        }
        if let Some(supers) = node.child_by_field_name("superclasses") {
            let text = node_text(supers, self.content);
            if !SUPERCLASSES_RE.is_match(text) {
                return None; // legacy charset would fail to match
            }
            end = supers.end_byte();
        }
        let start = self.space_indent_start(node.start_byte());
        Some(self.content[start..end].to_string())
    }

    fn handle_assignment(&mut self, node: Node<'a>) {
        let Some(left) = node.child_by_field_name("left") else {
            return;
        };
        if left.kind() != "identifier" {
            return;
        }
        let left_text = node_text(left, self.content);
        let stmt_start = left.start_byte();
        let indent_start = self.ws_indent_start(stmt_start);
        let at_col0 = indent_start == stmt_start && self.is_line_start(stmt_start);
        let line_region = &self.content[stmt_start..];

        if at_col0 {
            // legacy version/typevar patterns require a preceding newline;
            // a statement at byte 0 of the file can never match
            if stmt_start == 0 {
                return;
            }
            if left_text == "__version__" {
                if let Some(m) = VERSION_RE.find(line_region.split('\n').next().unwrap_or("")) {
                    self.components.push(m.as_str().to_string());
                    self.in_class = false;
                }
            } else if let Some(m) = TYPEVAR_RE.find(line_region) {
                self.components.push(m.as_str().to_string());
                self.in_class = false;
            }
            return;
        }

        // indented assignments: enum variants first (legacy alternation order)
        let type_annotation = node.child_by_field_name("type");
        let right = node.child_by_field_name("right");
        if ALLCAPS_RE.is_match(left_text) && type_annotation.is_none() {
            if let Some(right) = right {
                let value_start = right.start_byte();
                let value_text = &self.content[value_start..];
                // only `NAME = value` with whitespace-separated `=` matches
                let between = &self.content[left.end_byte()..value_start];
                let eq_ok = {
                    let trimmed = between.trim();
                    trimmed == "="
                        && between.starts_with(char::is_whitespace)
                        && between.ends_with(char::is_whitespace)
                };
                if eq_ok {
                    if let Some(m) = INT_VALUE_RE.find(value_text) {
                        let end = value_start + m.end();
                        self.components
                            .push(self.content[indent_start..end].to_string());
                        return;
                    }
                    if let Some(m) = STR_VALUE_RE.find(value_text) {
                        let end = value_start + m.end();
                        self.components
                            .push(self.content[indent_start..end].to_string());
                        return;
                    }
                    // multiline constructor value: [A-Z]\w*( ... ^\s+\)$
                    if right.kind() == "call" {
                        let func_ok = right
                            .child_by_field_name("function")
                            .map(|f| {
                                f.kind() == "identifier"
                                    && node_text(f, self.content)
                                        .starts_with(|c: char| c.is_ascii_uppercase())
                            })
                            .unwrap_or(false);
                        if func_ok {
                            let call_region = &self.content[value_start..right.end_byte()];
                            if let Some(m) = STRUCT_CLOSING_RE.find(call_region) {
                                let end = value_start + m.end();
                                self.components
                                    .push(self.content[indent_start..end].to_string());
                                return;
                            }
                        }
                    }
                }
            }
        }

        // dataclass fields only in class context, with a type annotation
        if self.in_class && type_annotation.is_some() {
            let line_start = self.ws_indent_start(stmt_start);
            if !self.is_line_start(line_start) {
                return;
            }
            let region = &self.content[line_start..];
            let with_indent = format!(
                "{}{}",
                &self.content[line_start..stmt_start],
                region[stmt_start - line_start..]
                    .split('\n')
                    .next()
                    .unwrap_or("")
            );
            // legacy: `^\s+\w+:\s+[\w\[\]\|\., ]+`
            let padded = format!(" {with_indent}"); // \s+ needs >= 1 char
            if let Some(m) = FIELD_RE.find(&padded) {
                let captured = &padded[1..m.end()];
                let component = captured.trim_start_matches('\n').trim_end_matches(' ');
                self.components.push(component.to_string());
            }
        }
    }
}
