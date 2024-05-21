{
  description = "A fully-featured minimalistic configurable rust calculator.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        cargoTOML = builtins.fromTOML (builtins.readFile ./Cargo.toml);
      in
      rec {
        devShell = pkgs.mkShell {
          inputsFrom = [ packages.calc ];
          packages = [ pkgs.gnumake ];
        };

        formatter = pkgs.nixpkgs-fmt;
        packages = rec {
          default = calc;
          calc = pkgs.rustPlatform.buildRustPackage {
            inherit (cargoTOML.package) version;

            pname = "calc";
            src = ./.;

            nativeBuildInputs = [ pkgs.makeWrapper ];
            postFixup = ''
              wrapProgram $out/bin/mini-calc \
                --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.gnuplot ]}"
            '';

            cargoLock.lockFile = ./Cargo.lock;
            meta.mainProgram = "mini-calc";
          };
        };
      });
}
