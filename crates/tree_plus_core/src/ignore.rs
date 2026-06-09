//! Ignore and glob handling, matching legacy `tree_plus_src/ignore.py`.
//!
//! Legacy semantics:
//! - `should_ignore(path, ignore, globs)` returns true when any *component*
//!   of the normalized path matches any ignore pattern via `fnmatch.fnmatch`,
//!   or when amortized globs are present and the path is not in the match set.
//! - `DEFAULT_IGNORE` is a fixed pattern set; user patterns are unioned with
//!   it unless `override` is set.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use regex::Regex;

/// Default ignore patterns (legacy `DEFAULT_IGNORE_FROZENSET`).
pub const DEFAULT_IGNORE: &[&str] = &[
    "__init__.py",
    "__pycache__",
    "._*",
    ".angular",
    ".cache",
    ".coverage",
    ".DS_Store",
    ".flake8",
    ".git",
    ".hypothesis",
    ".idea",
    ".ipynb_checkpoints",
    ".pytest_cache",
    ".rustc_info.json",
    ".vscode",
    "*_memmap",
    "*:Zone.Identifier",
    "*.a",
    "*.ai",
    "*.bak",
    "*.bin",
    "*.bz2",
    "*.chk",
    "*.class",
    "*.d",
    "*.dat",
    "*.dll",
    "*.dylib",
    "*.ear",
    "*.egg-info",
    "*.eot",
    "*.eps",
    "*.flac",
    "*.flv",
    "*.framework",
    "*.img",
    "*.ipa",
    "*.iso",
    "*.jar",
    "*.lib",
    "*.lock",
    "*.log",
    "*.nib",
    "*.node",
    "*.o",
    "*.obj",
    "*.odg",
    "*.pack",
    "*.psd",
    "*.pyc",
    "*.pyd",
    "*.pyo",
    "*.rar",
    "*.rlib",
    "*.rmeta",
    "*.so",
    "*.storyboardc",
    "*.swo",
    "*.swp",
    "*.tar",
    "*.tml",
    "*.ttf",
    "*.war",
    "*.woff",
    "*.xcarchive",
    "*.zip",
    "*.zst",
    "**/target/debug/**",
    "**/tmp/",
    "*~",
    "babel-webpack",
    "build",
    "CACHEDIR.TAG",
    "Cargo.lock",
    "detritus",
    "dist",
    "env",
    "node_modules",
    "target",
    "venv",
];

/// Translate a Python `fnmatch` pattern to an anchored regex string.
///
/// Supports `*`, `?`, `[seq]`, `[!seq]`; everything else is escaped, matching
/// `fnmatch.translate` semantics closely enough for the ignore pattern set.
pub fn fnmatch_translate(pattern: &str) -> String {
    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::from("(?s)^");
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        i += 1;
        match c {
            '*' => out.push_str(".*"),
            '?' => out.push('.'),
            '[' => {
                let mut j = i;
                if j < chars.len() && (chars[j] == '!' || chars[j] == ']') {
                    j += 1;
                }
                while j < chars.len() && chars[j] != ']' {
                    j += 1;
                }
                if j >= chars.len() {
                    out.push_str("\\[");
                } else {
                    let inner: String = chars[i..j].iter().collect();
                    let inner = inner.replace('\\', "\\\\");
                    out.push('[');
                    if let Some(rest) = inner.strip_prefix('!') {
                        out.push('^');
                        out.push_str(rest);
                    } else if inner.starts_with('^') {
                        out.push('\\');
                        out.push_str(&inner);
                    } else {
                        out.push_str(&inner);
                    }
                    out.push(']');
                    i = j + 1;
                }
            }
            other => {
                if "\\.+()|{}^$".contains(other) {
                    out.push('\\');
                }
                out.push(other);
            }
        }
    }
    out.push('$');
    out
}

/// A compiled set of ignore patterns.
#[derive(Debug, Clone)]
pub struct IgnorePatterns {
    regexes: Vec<Regex>,
}

impl IgnorePatterns {
    pub fn new<I: IntoIterator<Item = S>, S: AsRef<str>>(patterns: I) -> Self {
        let regexes = patterns
            .into_iter()
            .filter_map(|p| Regex::new(&fnmatch_translate(p.as_ref())).ok())
            .collect();
        IgnorePatterns { regexes }
    }

    pub fn is_empty(&self) -> bool {
        self.regexes.is_empty()
    }

    /// Whether any path component matches any pattern (legacy `should_ignore`).
    pub fn matches_path(&self, path: &Path) -> bool {
        if self.regexes.is_empty() {
            return false;
        }
        for part in path.iter() {
            let Some(part) = part.to_str() else { continue };
            if self.regexes.iter().any(|r| r.is_match(part)) {
                return true;
            }
        }
        false
    }
}

/// Build the effective ignore pattern list (legacy `parse_ignore`).
pub fn parse_ignore(user_patterns: &[String], override_ignore: bool) -> Vec<String> {
    let mut patterns: Vec<String> = user_patterns.to_vec();
    if !override_ignore {
        patterns.extend(DEFAULT_IGNORE.iter().map(|s| s.to_string()));
    }
    patterns.sort();
    patterns.dedup();
    patterns
}

/// Pre-resolved glob matches (legacy `AmortizedGlobs`): the set of every
/// matching path plus all ancestor directories.
#[derive(Debug, Clone)]
pub struct AmortizedGlobs {
    pub matches: HashSet<PathBuf>,
}

impl AmortizedGlobs {
    pub fn contains(&self, path: &Path) -> bool {
        self.matches.contains(path)
    }
}

/// Recursively find glob matches under `paths` (legacy `amortize_globs`).
///
/// Patterns are name globs (like `Path.rglob`), matched against each entry's
/// file name at any depth. Returns `None` when nothing matches.
pub fn amortize_globs(paths: &[PathBuf], globs: &[String]) -> Option<AmortizedGlobs> {
    if paths.is_empty() || globs.is_empty() {
        return None;
    }
    let regexes: Vec<Regex> = globs
        .iter()
        .filter_map(|g| Regex::new(&fnmatch_translate(g)).ok())
        .collect();
    let mut matches: HashSet<PathBuf> = HashSet::new();
    fn walk(dir: &Path, regexes: &[Regex], matches: &mut HashSet<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let p = entry.path();
            let name_matches = p
                .file_name()
                .and_then(|n| n.to_str())
                .map(|n| regexes.iter().any(|r| r.is_match(n)))
                .unwrap_or(false);
            if name_matches && !matches.contains(&p) {
                matches.insert(p.clone());
                for parent in p.ancestors().skip(1) {
                    matches.insert(parent.to_path_buf());
                }
            }
            if p.is_dir() {
                walk(&p, regexes, matches);
            }
        }
    }
    for path in paths {
        walk(path, &regexes, &mut matches);
    }
    if matches.is_empty() {
        return None;
    }
    Some(AmortizedGlobs { matches })
}

/// Combined skip decision (legacy `should_ignore`).
pub fn should_ignore(path: &Path, ignore: &IgnorePatterns, globs: Option<&AmortizedGlobs>) -> bool {
    if let Some(globs) = globs {
        if !globs.contains(path) {
            return true;
        }
    }
    ignore.matches_path(path)
}

/// Whether a string looks like a glob (legacy `is_glob`).
pub fn is_glob(x: &str) -> bool {
    static GLOB_RE: std::sync::LazyLock<Regex> =
        std::sync::LazyLock::new(|| Regex::new(r"(\*)?\*|\?|\[(.\-.)+\]").unwrap());
    GLOB_RE.is_match(x)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fnmatch_star_and_class() {
        let p = IgnorePatterns::new(["*.pyc"]);
        assert!(p.matches_path(Path::new("a/b/c.pyc")));
        assert!(!p.matches_path(Path::new("a/b/c.py")));
        let dot = IgnorePatterns::new(["._*"]);
        assert!(dot.matches_path(Path::new("x/._hidden")));
        assert!(!dot.matches_path(Path::new("x/h._idden_not_component")));
    }

    #[test]
    fn component_matching() {
        let p = IgnorePatterns::new(["target"]);
        assert!(p.matches_path(Path::new("crates/x/target/debug/foo")));
        assert!(!p.matches_path(Path::new("crates/x/targeted/foo")));
    }

    #[test]
    fn is_glob_examples() {
        assert!(is_glob("*.rs"));
        assert!(is_glob("file?.py"));
        assert!(!is_glob("plain.txt"));
    }

    #[test]
    fn parse_ignore_union() {
        let with_default = parse_ignore(&["*.java".to_string()], false);
        assert!(with_default.iter().any(|p| p == "*.java"));
        assert!(with_default.iter().any(|p| p == ".git"));
        let only_user = parse_ignore(&["*.java".to_string()], true);
        assert_eq!(only_user, vec!["*.java".to_string()]);
    }
}
