//! Natural path sorting compatible with `natsort.os_sorted` (POSIX, no PyICU).
//!
//! Empirically verified key structure (natsort 8.4.0):
//! - a path is split into components (by `/`);
//! - the final component is split into (base, suffix...) where at most the
//!   last TWO suffixes are split off, a suffix must be `.`-prefixed with
//!   total length <= 5 (e.g. `.json` yes, `.jsonl` no), and a purely numeric
//!   suffix (e.g. `.12345`) stops suffix splitting entirely;
//! - every piece is chunked into alternating (string, number, string, ...)
//!   starting with a (possibly empty) string chunk; digit runs become
//!   unsigned integers; text chunks compare case-insensitively (casefold).
//!
//! Ties (identical keys, e.g. names differing only by case) are broken by the
//! raw string to keep the order deterministic; the Python implementation
//! falls back to filesystem enumeration order here (documented difference).

use std::cmp::Ordering;

/// One chunk of a natural-sort key piece.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Chunk {
    Text(String),
    Num(u128),
}

impl Chunk {
    fn cmp_chunk(&self, other: &Chunk) -> Ordering {
        match (self, other) {
            (Chunk::Text(a), Chunk::Text(b)) => a.cmp(b),
            (Chunk::Num(a), Chunk::Num(b)) => a.cmp(b),
            // alternation makes mixed comparisons rare; numbers first matches
            // natsort's empty-string-prefix convention
            (Chunk::Num(_), Chunk::Text(_)) => Ordering::Less,
            (Chunk::Text(_), Chunk::Num(_)) => Ordering::Greater,
        }
    }
}

/// Natural sort key for one path-like string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OsSortKey {
    pieces: Vec<Vec<Chunk>>,
    raw: String,
}

impl PartialOrd for OsSortKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OsSortKey {
    fn cmp(&self, other: &Self) -> Ordering {
        let n = self.pieces.len().min(other.pieces.len());
        for i in 0..n {
            let a = &self.pieces[i];
            let b = &other.pieces[i];
            let m = a.len().min(b.len());
            for j in 0..m {
                match a[j].cmp_chunk(&b[j]) {
                    Ordering::Equal => {}
                    non_eq => return non_eq,
                }
            }
            match a.len().cmp(&b.len()) {
                Ordering::Equal => {}
                non_eq => return non_eq,
            }
        }
        match self.pieces.len().cmp(&other.pieces.len()) {
            Ordering::Equal => self.raw.cmp(&other.raw),
            non_eq => non_eq,
        }
    }
}

/// Chunk a string into alternating text/number chunks, starting with text.
fn chunk(s: &str) -> Vec<Chunk> {
    let mut chunks = Vec::new();
    let mut text = String::new();
    let mut num: Option<u128> = None;
    for ch in s.chars() {
        if ch.is_ascii_digit() {
            if num.is_none() {
                // flush text chunk (natsort keys always start with a string)
                chunks.push(Chunk::Text(std::mem::take(&mut text).to_lowercase()));
                num = Some(0);
            }
            let d = u128::from(ch as u8 - b'0');
            num = Some(num.unwrap().saturating_mul(10).saturating_add(d));
        } else {
            if let Some(n) = num.take() {
                chunks.push(Chunk::Num(n));
            }
            text.push(ch);
        }
    }
    if let Some(n) = num {
        chunks.push(Chunk::Num(n));
    } else {
        chunks.push(Chunk::Text(text.to_lowercase()));
    }
    chunks
}

/// Split the last path component into base + up to two short suffixes.
fn split_suffixes(name: &str) -> Vec<String> {
    // collect candidate suffixes from the right
    let mut parts: Vec<String> = Vec::new();
    let mut base = name.to_string();
    for _ in 0..2 {
        // a hidden-file leading dot is not a suffix boundary
        let search_region = &base[1.min(base.len())..];
        let Some(dot_rel) = search_region.rfind('.') else {
            break;
        };
        let dot = dot_rel + 1;
        let suffix = &base[dot..];
        // suffix includes the dot; must be <= 5 chars and not purely numeric
        if suffix.len() > 5 || suffix.len() < 2 {
            break;
        }
        if suffix[1..].chars().all(|c| c.is_ascii_digit()) {
            break;
        }
        parts.push(suffix.to_string());
        base.truncate(dot);
        if base.ends_with('.') {
            base.pop();
        }
    }
    let mut out = vec![base];
    out.extend(parts.into_iter().rev());
    out
}

/// Build the natural sort key for a path-like string.
pub fn os_sort_key(path: &str) -> OsSortKey {
    let mut pieces: Vec<Vec<Chunk>> = Vec::new();
    let components: Vec<&str> = path.split('/').filter(|c| !c.is_empty()).collect();
    for (i, comp) in components.iter().enumerate() {
        if i + 1 == components.len() {
            for piece in split_suffixes(comp) {
                pieces.push(chunk(&piece));
            }
        } else {
            pieces.push(chunk(comp));
        }
    }
    if pieces.is_empty() {
        pieces.push(chunk(path));
    }
    OsSortKey {
        pieces,
        raw: path.to_string(),
    }
}

/// Sort strings like `natsort.os_sorted`.
pub fn os_sorted<T: AsRef<str>>(items: &mut [T]) {
    items.sort_by_key(|a| os_sort_key(a.as_ref()));
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sorted_strs(mut v: Vec<&str>) -> Vec<&str> {
        v.sort_by_key(|a| os_sort_key(a));
        v
    }

    #[test]
    fn numeric_runs_sort_naturally() {
        assert_eq!(
            sorted_strs(vec!["file10.py", "file2.py"]),
            vec!["file2.py", "file10.py"]
        );
    }

    #[test]
    fn case_insensitive() {
        assert_eq!(
            sorted_strs(vec!["CUSTOMER-INVOICE.CBL", "addamt.cobol"]),
            vec!["addamt.cobol", "CUSTOMER-INVOICE.CBL"]
        );
        assert_eq!(
            sorted_strs(vec!["LuaTest.lua", "lesson.cbl", "KotlinTest.kt"]),
            vec!["KotlinTest.kt", "lesson.cbl", "LuaTest.lua"]
        );
    }

    #[test]
    fn suffix_split_rules() {
        // ".py" suffix split means "file.py" < "file2.py" (prefix tuple wins)
        assert_eq!(
            sorted_strs(vec!["file2.py", "file.py"]),
            vec!["file.py", "file2.py"]
        );
        // long suffixes are not split: a.json < a.jsonl
        assert_eq!(
            sorted_strs(vec!["a.jsonl", "a.json"]),
            vec!["a.json", "a.jsonl"]
        );
    }

    #[test]
    fn dotfiles_sort_before_letters() {
        assert_eq!(
            sorted_strs(vec!["claude.md", ".github", ".env.test"]),
            vec![".env.test", ".github", "claude.md"]
        );
    }
}
