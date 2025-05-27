{
  description = "A Fully-Featured Configurable (mini) Rust Calculator.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in rec {
      devShell = pkgs.mkShell {
        inputsFrom = [packages.calc];
        packages = [
          pkgs.gnumake
          pkgs.rust-analyzer
          pkgs.rustfmt
        ];
      };

      formatter = pkgs.alejandra;

      packages = {
        default = packages.calc;
        calc = pkgs.callPackage ./nix/package.nix {};
      };
    });
}
