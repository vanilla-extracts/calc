.PHONY: clean
SHELL=/bin/bash

build:
	cargo build
release:
	cargo build --release
clean:
	cargo clean
watch: build
	cargo watch -x fmt -x run
