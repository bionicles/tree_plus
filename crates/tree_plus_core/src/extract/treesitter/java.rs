//! Java component extraction (tree-sitter), matching legacy `parse_java`.
//!
//! The legacy extractor was one combined regex over comment-stripped text:
//! - classes: `(public )?(abstract )?class NAME( extends \w+)?( implements
//!   [\w, ]+)?` followed by `\s*{`, preceded by a newline;
//! - methods/constructors: optional visibility, optional abstract/static,
//!   optional one-word return type, `name(args)` followed by exactly ` {\n`;
//! - interfaces: emitted as `interface NAME` (modifiers dropped);
//! - annotations on their own lines: ` *@[\w"/()]+` (truncates at the first
//!   character outside that set);
//! - bodyless methods: ` *(abstract)? \w+ \w+(args);` (exactly two words, so
//!   `public String f();` never matched).
//!
//! Annotations inside parameter lists were consumed by the method match and
//! never emitted; we reproduce that by not descending into parameters.

use std::sync::LazyLock;

use regex::Regex;
use tree_sitter::Node;

use super::rust::strip_c_comments;
use super::{parse, ExtractResult};

static CLASS_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"^( *(?:public )?(?:abstract )?class\s+\w+(?: extends \w+)?(?: implements [\w, ]+)?)\s*$",
    )
    .unwrap()
});

static INTERFACE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ *(?:public )?interface\s+(\w+)").unwrap());

static METHOD_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?s)^ *(?:public|protected|private)? ?(?:abstract|static)? ?(?:\w+ )?\w+\([^{]*\)$",
    )
    .unwrap()
});

static ABSTRACT_METHOD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?s)^ *(?:abstract)? \w+ \w+\([^)]*\)$").unwrap());

static ANNOTATION_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"^ *@[\w"/()]+"#).unwrap());

/// Extract Java components: classes, interfaces, methods, annotations.
pub fn extract(content: &str) -> ExtractResult {
    let tree = parse(content, &tree_sitter_java::LANGUAGE.into())?;
    let mut extractor = JavaExtractor {
        content,
        components: Vec::new(),
    };
    extractor.run(tree.root_node());
    Ok(extractor.components)
}

struct JavaExtractor<'a> {
    content: &'a str,
    components: Vec<String>,
}

fn line_start(content: &str, byte: usize) -> usize {
    content[..byte].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

impl<'a> JavaExtractor<'a> {
    /// Depth-first via an explicit stack: AST depth is input-controlled, and
    /// extraction runs on small worker-thread stacks.
    fn run(&mut self, root: Node<'a>) {
        let mut stack = vec![root];
        while let Some(node) = stack.pop() {
            match node.kind() {
                "class_declaration" | "enum_declaration" | "record_declaration" => {
                    self.emit_annotations(node);
                    if node.kind() == "class_declaration" {
                        self.emit_class(node);
                    }
                    if let Some(body) = node.child_by_field_name("body") {
                        stack.push(body);
                    }
                }
                "interface_declaration" | "annotation_type_declaration" => {
                    self.emit_annotations(node);
                    if node.kind() == "interface_declaration" {
                        self.emit_interface(node);
                    }
                    if let Some(body) = node.child_by_field_name("body") {
                        stack.push(body);
                    }
                }
                "method_declaration" | "constructor_declaration" => {
                    self.emit_annotations(node);
                    match node.child_by_field_name("body") {
                        Some(body) => {
                            self.emit_method(node, body);
                            // local/anonymous classes inside bodies still match
                            stack.push(body);
                        }
                        None => self.emit_bodyless_method(node),
                    }
                }
                "field_declaration" => {
                    self.emit_annotations(node);
                }
                _ => {
                    let mut cursor = node.walk();
                    let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();
                    stack.extend(children.into_iter().rev());
                }
            }
        }
    }

    /// Annotations attached as modifiers, one component per annotation line.
    fn emit_annotations(&mut self, node: Node<'a>) {
        let mut cursor = node.walk();
        let Some(mods) = node.children(&mut cursor).find(|c| c.kind() == "modifiers") else {
            return;
        };
        let mut mcursor = mods.walk();
        for ann in mods.named_children(&mut mcursor) {
            if !matches!(ann.kind(), "annotation" | "marker_annotation") {
                continue;
            }
            let start = line_start(self.content, ann.start_byte());
            let slice = &self.content[start..ann.end_byte()];
            if let Some(m) = ANNOTATION_RE.find(slice) {
                self.components.push(m.as_str().to_string());
            }
        }
    }

    /// Byte offset where the signature text begins: the line after the last
    /// annotation modifier (legacy matches started past the annotations).
    fn signature_line_start(&self, node: Node<'a>) -> usize {
        let mut pos = node.start_byte();
        let mut cursor = node.walk();
        if let Some(mods) = node.children(&mut cursor).find(|c| c.kind() == "modifiers") {
            let mut mcursor = mods.walk();
            for ann in mods.named_children(&mut mcursor) {
                if matches!(ann.kind(), "annotation" | "marker_annotation") {
                    pos = pos.max(ann.end_byte());
                }
            }
        }
        let rest = &self.content[pos..];
        let skipped = rest.len() - rest.trim_start().len();
        line_start(self.content, pos + skipped)
    }

    fn emit_class(&mut self, node: Node<'a>) {
        let Some(body) = node.child_by_field_name("body") else {
            return;
        };
        let start = self.signature_line_start(node);
        if start == 0 {
            return; // legacy required a preceding newline
        }
        let candidate = strip_c_comments(&self.content[start..body.start_byte()]);
        // stripped comment lines between annotations and the signature
        // leave leading newlines behind (legacy stripped globally)
        let candidate = candidate.trim_start_matches('\n');
        if let Some(caps) = CLASS_RE.captures(candidate) {
            self.components
                .push(caps.get(1).unwrap().as_str().trim_end().to_string());
        }
    }

    fn emit_interface(&mut self, node: Node<'a>) {
        let Some(body) = node.child_by_field_name("body") else {
            return;
        };
        let start = self.signature_line_start(node);
        if start == 0 {
            return;
        }
        let candidate = strip_c_comments(&self.content[start..body.start_byte()]);
        let candidate = candidate.trim_start_matches('\n');
        if let Some(caps) = INTERFACE_RE.captures(candidate) {
            self.components.push(format!("interface {}", &caps[1]));
        }
    }

    fn emit_method(&mut self, node: Node<'a>, body: Node<'a>) {
        let start = self.signature_line_start(node);
        if start == 0 {
            return;
        }
        // legacy gate: the signature's `)` is followed by exactly ` {\n`
        let before = &self.content[start..body.start_byte()];
        if !before.ends_with(' ') || before.ends_with("  ") || before.ends_with("\n ") {
            return;
        }
        if !self.content[body.start_byte() + 1..].starts_with('\n') {
            return;
        }
        let candidate = strip_c_comments(before);
        let candidate = candidate.trim_start_matches('\n').trim_end();
        if METHOD_RE.is_match(candidate) {
            self.components.push(candidate.to_string());
        }
    }

    /// Bodyless (abstract/interface) methods: exactly two words + `(args);`.
    fn emit_bodyless_method(&mut self, node: Node<'a>) {
        let start = self.signature_line_start(node);
        if start == 0 {
            return;
        }
        let raw = self.content[start..node.end_byte()].trim_end();
        let Some(raw) = raw.strip_suffix(';') else {
            return;
        };
        let candidate = strip_c_comments(raw);
        if ABSTRACT_METHOD_RE.is_match(&candidate) {
            self.components.push(candidate);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn class_method_annotation() {
        let src = "\nabstract class A {\n    abstract void f();\n}\n\n@Log\nclass B extends A {\n    @Override\n    void f() {\n        g();\n    }\n}\n";
        let got = extract(src).unwrap();
        assert_eq!(
            got,
            vec![
                "abstract class A",
                "    abstract void f()",
                "@Log",
                "class B extends A",
                "    @Override",
                "    void f()",
            ]
        );
    }

    #[test]
    fn interface_drops_modifiers() {
        let src = "\npublic interface Comm {\n    String communicate();\n}\n";
        let got = extract(src).unwrap();
        assert_eq!(got, vec!["interface Comm", "    String communicate()"]);
    }

    #[test]
    fn three_word_prototype_skipped() {
        // legacy two-word rule: `public String f();` never matched
        let src = "\ninterface I {\n    public String f();\n}\n";
        let got = extract(src).unwrap();
        assert_eq!(got, vec!["interface I"]);
    }

    #[test]
    fn single_line_body_skipped() {
        // legacy required ` {` followed by a newline
        let src = "\nclass A {\n    void f() { g(); }\n}\n";
        let got = extract(src).unwrap();
        assert_eq!(got, vec!["class A"]);
    }
}
