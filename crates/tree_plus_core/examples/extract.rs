//! Debug helper: print extracted components as JSON for given paths.
//! Usage: cargo run -p tree_plus_core --example extract -- <path>...

fn main() {
    for arg in std::env::args().skip(1) {
        let components = tree_plus_core::extract_components(std::path::Path::new(&arg), false);
        println!(
            "{}",
            serde_json::to_string_pretty(&serde_json::json!({
                "path": arg,
                "components": components,
            }))
            .unwrap()
        );
    }
}
