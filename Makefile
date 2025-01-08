watch:
	cargo watch -x fmt -x run
compile:
	cargo build
release: 
	cargo build --release
test:
	cargo test
publish:
	cargo publish
