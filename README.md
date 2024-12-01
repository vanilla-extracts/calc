# Calc

<div align="center">

***Calc: A Fully-Featured Configurable (mini) Rust Algebric Calculator***

[![Rust Test](https://github.com/coco33920/calc/actions/workflows/rust-test.yml/badge.svg)](https://github.com/coco33920/calc/actions/workflows/rust-test.yml)
[![Release](https://img.shields.io/github/v/release/coco33920/calc.svg?include_prereleases=&sort=semver&color=f7a8d8)](https://github.com/coco33920/calc/releases/latest)
[![](https://img.shields.io/crates/v/mini-calc?link=https%3A%2F%2Fcrates.io%2Fcrates%2Fmini-calc)](https://crates.io/crates/mini-calc)
![](https://img.shields.io/crates/l/mini-calc?link=https%3A%2F%2Fgithub.com%2coco33920%2Fcalc%2Fblob%2Fmaster%2FLICENCE)
[![](https://img.shields.io/crates/d/mini-calc)](https://crates.io/crates/mini-calc)

</div>

## Install

### Source 

You can install the latest version from source

```bash
git clone https://github.com/vanilla-extracts/calc
cd calc
cargo build --release
./target/release/mini-calc
```

### Cargo
```bash
cargo install mini-calc
```

### using Nix

Alternatively, you can use [nix](https://nixos.org) to build or run this project.

Running directly:
```sh
nix run github:vanilla-extracts/calc
```
> You may need to enable nix experimental features. In that case, execute the following command: `mkdir -p ~/.config/nix && echo "experimental-features = nix-command flakes" | tee ~/.config/nix/nix.conf`

### From packaged version
Both `.deb` and `.rpm` packages are availables in the download page

## Website
The website (powered by oranda) is available for more informations [the website](https://calc.charlotte-thomas.me) for more informations.

There is a _MD Book_ [there](https://calc.charlotte-thomas.me/book)

## Manual

If you prefer a PDF, there is a [manual](https://calc.charlotte-thomas.me/assets/manual.pdf)

## Contributors

|                                                                                                                                               | Name    | Role                     | Website                                     |
|-------------------------------------------------------------------------------------------------------------------------------------------------|:--------|:-------------------------|:--------------------------------------------|
| [<img src="https://avatars.githubusercontent.com/u/17108449?v=4" style="border-radius: 50%;height:90pt;width:auto">](https://github.com/vanilla-extracts)        |Charlotte THOMAS          | Main developer/Maintener | [Main page](https://www.charlotte-thomas.me)         | 
| [<img src="https://avatars.githubusercontent.com/u/87855546?v=4" style="border-radius: 50%;height:90pt;width:auto">](https://github.com/leana8959)        |Léana 江                  | Help, cleanup            | [Website/Blog](https://earth2077.fr)        |
| [<img src="https://avatars.githubusercontent.com/u/53050011?v=4" style="border-radius:50%;height:90pt;width:auto">](https://github.com/Sigmaficient)      |Sigmaficient              | Nixify                   | [Website](https://sigmanificient.github.io/)|
