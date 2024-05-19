watch:
	cargo watch -x check -x fmt -x run
compile:
	cargo build
release: 
	cargo build --release
test:
	cargo test
publish:
	cargo publish
