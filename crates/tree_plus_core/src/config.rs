//! Configuration for tree building (CLI flags map onto this).

use crate::count::TokenizerName;

/// Maximum token count before a file's components are skipped
/// (legacy `MAX_TOKENS`).
pub const MAX_TOKENS: u64 = 1_000_000;

#[derive(Debug, Clone)]
pub struct TreePlusConfig {
    /// User ignore patterns (unioned with defaults unless `override_ignore`).
    pub ignore: Vec<String>,
    /// Use only user ignore patterns.
    pub override_ignore: bool,
    /// Glob patterns to keep (e.g. `*.rs`).
    pub globs: Vec<String>,
    /// Skip component extraction entirely.
    pub concise: bool,
    /// Tokenizer (only `Wc` is supported in the Rust port).
    pub tokenizer: TokenizerName,
    /// Legacy syntax-highlighting flag; affects Rust enum markup escaping.
    pub syntax: bool,
    /// Maximum tokens before skipping component extraction.
    pub max_tokens: u64,
}

impl Default for TreePlusConfig {
    fn default() -> Self {
        TreePlusConfig {
            ignore: Vec::new(),
            override_ignore: false,
            globs: Vec::new(),
            concise: false,
            tokenizer: TokenizerName::Wc,
            syntax: false,
            max_tokens: MAX_TOKENS,
        }
    }
}
