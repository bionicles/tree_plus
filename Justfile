# tree_plus Rust port

# install the `tprs` alias binary (avoids PATH collision with Python tree_plus)
install:
	cargo install --path crates/tree_plus_cli --bin tprs

# install both binaries: `tree_plus` and `tprs`
install-all:
	cargo install --path crates/tree_plus_cli

test:
	cargo test --workspace --all-features

lint:
	cargo fmt --all -- --check
	cargo clippy --all-targets --all-features -- -D warnings

# regenerate legacy goldens from the Python implementation
goldens:
	python tests/golden/generate_legacy_goldens.py

bench:
	cargo bench -p tree_plus_core
