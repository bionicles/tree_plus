//! Core data model: `TreePlus` and `Category`.
//!
//! Mirrors the Python `tree_plus_src.engine.TreePlus` dataclass for the
//! version-1 scope (local filesystem mode: ROOT, GLOB, FOLDER, FILE,
//! COMPONENT categories; URL/TAG are deferred web-mode categories).

/// Category of a `TreePlus` node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Category {
    Root,
    Glob,
    Folder,
    File,
    Component,
}

impl Category {
    pub fn as_str(self) -> &'static str {
        match self {
            Category::Root => "root",
            Category::Glob => "glob",
            Category::Folder => "folder",
            Category::File => "file",
            Category::Component => "component",
        }
    }
}

/// A node in the tree_plus tree.
///
/// `components` holds the extracted display labels for FILE nodes
/// (the legacy Python stored these in `subtrees` as `List[str]`).
#[derive(Debug, Clone)]
pub struct TreePlus {
    pub category: Category,
    pub name: String,
    pub line_count: u64,
    pub token_count: u64,
    /// Child trees (folders/files/globs under folders or root).
    pub subtrees: Vec<TreePlus>,
    /// Extracted component labels (only for FILE nodes).
    pub components: Vec<String>,
}

impl TreePlus {
    pub fn new(category: Category, name: impl Into<String>) -> Self {
        TreePlus {
            category,
            name: name.into(),
            line_count: 0,
            token_count: 0,
            subtrees: Vec::new(),
            components: Vec::new(),
        }
    }

    pub fn is_folder(&self) -> bool {
        self.category == Category::Folder
    }

    pub fn is_file(&self) -> bool {
        self.category == Category::File
    }

    /// Total folder count including self (legacy `n_folders`).
    pub fn n_folders(&self) -> u64 {
        let own = u64::from(self.is_folder());
        self.subtrees.iter().map(TreePlus::n_folders).sum::<u64>() + own
    }

    /// Total file count (legacy `n_files`).
    pub fn n_files(&self) -> u64 {
        let own = u64::from(self.is_file());
        self.subtrees.iter().map(TreePlus::n_files).sum::<u64>() + own
    }

    /// Total line count (legacy `n_lines`).
    pub fn n_lines(&self) -> u64 {
        self.line_count + self.subtrees.iter().map(TreePlus::n_lines).sum::<u64>()
    }

    /// Total token count (legacy `n_tokens`).
    pub fn n_tokens(&self) -> u64 {
        self.token_count + self.subtrees.iter().map(TreePlus::n_tokens).sum::<u64>()
    }

    /// Structured representation for `--json` output. File nodes carry
    /// their own counts and components; folder/root counts are the
    /// aggregates so consumers don't have to re-derive them.
    pub fn to_json(&self) -> serde_json::Value {
        let mut obj = serde_json::Map::new();
        obj.insert("category".into(), self.category.as_str().into());
        obj.insert("name".into(), self.name.clone().into());
        obj.insert("n_folders".into(), self.n_folders().into());
        obj.insert("n_files".into(), self.n_files().into());
        obj.insert("n_lines".into(), self.n_lines().into());
        obj.insert("n_tokens".into(), self.n_tokens().into());
        if self.is_file() {
            obj.insert("components".into(), self.components.clone().into());
        }
        if !self.is_file() {
            obj.insert(
                "subtrees".into(),
                self.subtrees
                    .iter()
                    .map(TreePlus::to_json)
                    .collect::<Vec<_>>()
                    .into(),
            );
        }
        serde_json::Value::Object(obj)
    }

    /// Legacy `stats()` string, e.g. `1 folder(s), 6 file(s), 1,234 line(s), 5,678 token(s)`.
    pub fn stats(&self) -> String {
        format!(
            "{} folder(s), {} file(s), {} line(s), {} token(s)",
            commafy(self.n_folders()),
            commafy(self.n_files()),
            commafy(self.n_lines()),
            commafy(self.n_tokens()),
        )
    }
}

/// Format an integer with thousands separators like Python's `{:,}`.
pub fn commafy(n: u64) -> String {
    let digits = n.to_string();
    let mut out = String::with_capacity(digits.len() + digits.len() / 3);
    let offset = digits.len() % 3;
    for (i, ch) in digits.chars().enumerate() {
        if i != 0 && (i + 3 - offset).is_multiple_of(3) {
            out.push(',');
        }
        out.push(ch);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commafy_matches_python_format() {
        assert_eq!(commafy(0), "0");
        assert_eq!(commafy(999), "999");
        assert_eq!(commafy(1000), "1,000");
        assert_eq!(commafy(1234567), "1,234,567");
    }

    #[test]
    fn counts_roll_up() {
        let mut folder = TreePlus::new(Category::Folder, "f");
        let mut file = TreePlus::new(Category::File, "a.py");
        file.line_count = 10;
        file.token_count = 100;
        folder.subtrees.push(file);
        assert_eq!(folder.n_folders(), 1);
        assert_eq!(folder.n_files(), 1);
        assert_eq!(folder.n_lines(), 10);
        assert_eq!(folder.n_tokens(), 100);
    }
}
