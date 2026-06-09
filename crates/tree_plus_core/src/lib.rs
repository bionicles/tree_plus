//! tree_plus_core: a `tree` util enhanced with tokens, lines, and components.
//!
//! Rust port of the Python `tree_plus` package (version-1 scope: local
//! filesystem mode). See docs/architecture.md for the design and
//! docs/rust-port-differences.md for intentional differences.

pub mod config;
pub mod count;
pub mod extract;
pub mod ignore;
pub mod model;
pub mod render;
pub mod sort;
pub mod walk;

pub use config::TreePlusConfig;
pub use count::{count_tokens_lines, TokenLineCount, TokenizerName};
pub use extract::extract_components;
pub use ignore::DEFAULT_IGNORE;
pub use model::{Category, TreePlus};
pub use render::{render_to_string, DEFAULT_WIDTH};
pub use walk::from_seeds;

impl TreePlus {
    /// Render this tree exactly like the legacy `TreePlus.into_str()`.
    pub fn into_str(&self) -> String {
        render::render_to_string(self, render::DEFAULT_WIDTH)
    }
}
