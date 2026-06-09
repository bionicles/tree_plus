//! tree_plus CLI: a `tree` util enhanced with tokens, lines, and components.
//!
//! Flag-compatible with the legacy Python click CLI for the version-1 scope
//! (local filesystem mode). Web/HN modes are deferred; see
//! docs/rust-port-differences.md.

use std::time::Instant;

use clap::Parser;

use tree_plus_core::count::TokenizerName;
use tree_plus_core::{from_seeds, render_to_string, TreePlusConfig};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser, Debug)]
#[command(
    name = "tree_plus",
    disable_version_flag = true,
    about = "A `tree` util enhanced with tokens, lines, and components.",
    after_help = format!("v({VERSION}) --- https://github.com/bionicles/tree_plus/blob/main/README.md"),
)]
struct Cli {
    /// Patterns to ignore, in quotes: -i "*.java"
    #[arg(short = 'i', long = "ignore")]
    ignore: Vec<String>,

    /// Override DEFAULT_IGNORE (includes ignored content): -o -i "*.java"
    #[arg(short = 'o', long = "override")]
    override_ignore: bool,

    /// Patterns to find, in quotes: -g "*.rs"
    #[arg(short = 'g', long = "glob")]
    glob: Vec<String>,

    /// Print the version and exit.
    #[arg(short = 'v', long = "version")]
    version: bool,

    /// Enables debug output.
    #[arg(short = 'd', long = "debug")]
    debug: bool,

    /// DISABLE Syntax Highlighting. (The Rust port renders plain text; this
    /// flag is accepted for compatibility and controls legacy markup escaping.)
    #[arg(short = 's', long = "syntax")]
    syntax: bool,

    /// Omit module components. (False)
    #[arg(short = 'c', long = "concise")]
    concise: bool,

    /// Emit the tree as JSON instead of rendering it (machine-readable;
    /// no footer). File nodes carry components; counts are aggregated.
    #[arg(short = 'j', long = "json")]
    json: bool,

    /// A shorthand for tiktoken with the 'gpt-4o' tokenizer (unsupported in
    /// the Rust port: errors explicitly).
    #[arg(short = 't', long = "tiktoken")]
    tiktoken: bool,

    /// Name of the tokenizer to use; only 'wc' is supported in the Rust port.
    #[arg(short = 'T', long = "tokenizer-name")]
    tokenizer_name: Option<String>,

    /// Regex timeout in seconds (accepted for compatibility; the Rust port
    /// does not need regex timeouts).
    #[arg(long = "timeout")]
    timeout: Option<f64>,

    /// Paths or globs to map.
    paths: Vec<String>,
}

fn main() {
    let start = Instant::now();
    let cli = Cli::parse();

    if cli.version {
        println!("{VERSION}");
        return;
    }

    let tokenizer = match (cli.tiktoken, cli.tokenizer_name.as_deref()) {
        (false, None) | (true, Some("wc")) | (false, Some("wc")) => TokenizerName::Wc,
        (true, None) | (_, Some("gpt4o")) | (_, Some("gpt-4o")) => {
            eprintln!(
                "error: the gpt-4o tokenizer is not supported by the Rust port; \
                 use the default word-count tokenizer (wc)"
            );
            std::process::exit(2);
        }
        (_, Some(other)) => {
            eprintln!("error: unsupported tokenizer {other:?} (only 'wc' is supported)");
            std::process::exit(2);
        }
    };

    let config = TreePlusConfig {
        ignore: cli.ignore.clone(),
        override_ignore: cli.override_ignore,
        globs: cli.glob.clone(),
        concise: cli.concise,
        tokenizer,
        syntax: false,
        max_tokens: tree_plus_core::config::MAX_TOKENS,
    };
    if cli.debug {
        eprintln!(
            "tree_plus main received paths={:?} ignore={:?} glob={:?}",
            cli.paths, cli.ignore, cli.glob
        );
    }
    let _ = cli.timeout; // compatibility no-op
    let _ = cli.syntax; // plain-text output; highlighting deferred

    let root = from_seeds(&cli.paths, &config);

    if cli.json {
        let json = serde_json::to_string_pretty(&root.to_json()).expect("serialize tree");
        println!("{json}");
        return;
    }

    let width = terminal_width();
    print!("{}", render_to_string(&root, width));

    let og_ignore = format_tuple(&cli.ignore);
    let globs = format_tuple(&cli.glob);
    let paths = format_tuple(&cli.paths);
    let mut footer = format!("\ntree_plus v({VERSION}) ignore={og_ignore} globs={globs}");
    if cli.concise {
        footer.push_str(&format!(" concise=True paths={paths}"));
    } else {
        footer.push_str(&format!(" syntax={} paths={paths}", py_bool(!cli.syntax)));
    }
    footer.push_str(&format!(
        "\n{} in {:.2} second(s).",
        root.stats(),
        start.elapsed().as_secs_f64()
    ));
    println!("{footer}");
}

/// Python tuple repr for the footer line, e.g. `('a',)` / `()` / `None`.
fn format_tuple(items: &[String]) -> String {
    if items.is_empty() {
        return "()".to_string();
    }
    let inner: Vec<String> = items.iter().map(|s| format!("'{s}'")).collect();
    if items.len() == 1 {
        format!("({},)", inner[0])
    } else {
        format!("({})", inner.join(", "))
    }
}

fn py_bool(b: bool) -> &'static str {
    if b {
        "True"
    } else {
        "False"
    }
}

fn terminal_width() -> usize {
    use std::io::IsTerminal;
    // honor the legacy README-update width override
    if std::env::var("TREE_PLUS_UPDATE_README").as_deref() == Ok("YES") {
        return 128;
    }
    // piped output renders at the legacy capture width
    if !std::io::stdout().is_terminal() {
        return tree_plus_core::DEFAULT_WIDTH;
    }
    // real terminal width, like the legacy rich Console
    if let Some((terminal_size::Width(w), _)) = terminal_size::terminal_size() {
        if w > 0 {
            return w as usize;
        }
    }
    tree_plus_core::DEFAULT_WIDTH
}
