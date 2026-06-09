//! Token and line counting, matching the legacy `wc -ml`-based counter.
//!
//! Legacy default tokenizer ("wc"): `n_lines` = newline count, `n_tokens` =
//! character count / 4 (1 token ~= 4 chars). Characters are Unicode scalar
//! values for valid UTF-8; invalid bytes each count as one character, which
//! matches `wc -m` behavior in a UTF-8 locale closely enough for parity.

use std::collections::HashSet;
use std::path::Path;
use std::sync::LazyLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct TokenLineCount {
    pub n_tokens: u64,
    pub n_lines: u64,
}

/// Tokenizer selection mirroring the legacy `TokenizerName` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TokenizerName {
    #[default]
    Wc,
    /// Recognized but unsupported in the Rust port; selecting it is an
    /// explicit error per the port requirements.
    Gpt4o,
}

/// Extensions whose contents are never counted (legacy `extensions_not_to_count`).
static EXTENSIONS_NOT_TO_COUNT: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    [
        ".stl",
        ".obj",
        ".gcode",
        ".3mf",
        ".amf",
        ".ply",
        ".f3d",
        ".iges",
        ".igs",
        "step",
        ".stp",
        ".vrml",
        ".wrl",
        ".7z",
        ".aac",
        ".ai",
        ".avi",
        ".bak",
        ".bin",
        ".bz2",
        ".chk",
        ".class",
        ".csv",
        ".d",
        ".dat",
        ".db",
        ".dll",
        ".doc",
        ".docx",
        ".dylib",
        ".ear",
        ".eps",
        ".exe",
        ".flac",
        ".flv",
        ".framework",
        ".gdoc",
        ".gif",
        ".gsheet",
        ".gz",
        ".img",
        ".ipa",
        ".iso",
        ".jar",
        ".jpg",
        ".jpeg",
        ".lock",
        ".log",
        ".mov",
        ".mp3",
        ".mp4",
        ".nib",
        ".node",
        ".o",
        ".odg",
        ".pack",
        ".pdf",
        ".png",
        ".ppt",
        ".pptx",
        ".psd",
        ".pyc",
        ".pyo",
        ".pyd",
        ".rar",
        ".rlib",
        ".rmeta",
        ".so",
        ".sqlite",
        ".storyboardc",
        ".swp",
        ".tar",
        ".tml",
        ".wav",
        ".war",
        ".wmv",
        ".xcarchive",
        ".xlsx",
        ".zip",
        ".zst",
    ]
    .into_iter()
    .collect()
});

/// Lowercased extension including the dot, like `os.path.splitext` (lowered).
pub fn dot_extension(path: &Path) -> String {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    // os.path.splitext: leading dots do not start an extension
    let trimmed = name.trim_start_matches('.');
    let n_leading = name.len() - trimmed.len();
    match trimmed.rfind('.') {
        Some(i) => name[n_leading + i..].to_lowercase(),
        None => String::new(),
    }
}

/// Count tokens and lines for raw file bytes.
pub fn count_tokens_lines_bytes(bytes: &[u8]) -> TokenLineCount {
    let n_lines = bytes.iter().filter(|&&b| b == b'\n').count() as u64;
    // count UTF-8 scalar values; invalid bytes count 1 each (lossy)
    let mut n_chars: u64 = 0;
    let mut rest = bytes;
    while !rest.is_empty() {
        match std::str::from_utf8(rest) {
            Ok(s) => {
                n_chars += s.chars().count() as u64;
                break;
            }
            Err(e) => {
                let valid = e.valid_up_to();
                n_chars += std::str::from_utf8(&rest[..valid])
                    .map(|s| s.chars().count() as u64)
                    .unwrap_or(0);
                let skip = e.error_len().unwrap_or(rest.len() - valid).max(1);
                n_chars += 1; // the invalid sequence counts as one char
                rest = &rest[valid + skip..];
            }
        }
    }
    TokenLineCount {
        n_tokens: n_chars / 4,
        n_lines,
    }
}

/// Whether counting is skipped for this path (legacy `extensions_not_to_count`).
pub fn skip_counting(path: &Path) -> bool {
    EXTENSIONS_NOT_TO_COUNT.contains(dot_extension(path).as_str())
}

/// Count tokens and lines in a file; `None` when counting is skipped.
pub fn count_tokens_lines(path: &Path) -> Option<TokenLineCount> {
    if path.is_dir() || skip_counting(path) {
        return None;
    }
    let bytes = std::fs::read(path).ok()?;
    Some(count_tokens_lines_bytes(&bytes))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wc_style_counts() {
        let c = count_tokens_lines_bytes(b"def hello_world():\n    print(\"hello world\")\n");
        // 44 chars // 4 = 11 tokens, 2 newlines
        assert_eq!(c.n_lines, 2);
        assert_eq!(c.n_tokens, 11);
    }

    #[test]
    fn empty_is_zero() {
        let c = count_tokens_lines_bytes(b"");
        assert_eq!(c, TokenLineCount::default());
    }

    #[test]
    fn extension_skips() {
        assert!(skip_counting(Path::new("x/a.csv")));
        assert!(skip_counting(Path::new("a.SQLITE")));
        assert!(!skip_counting(Path::new("a.py")));
    }

    #[test]
    fn dot_extension_rules() {
        assert_eq!(dot_extension(Path::new("a.PY")), ".py");
        assert_eq!(dot_extension(Path::new(".env")), "");
        assert_eq!(dot_extension(Path::new(".env.test")), ".test");
        assert_eq!(dot_extension(Path::new("Makefile")), "");
    }
}
