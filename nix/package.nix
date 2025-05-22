{
  rustPlatform,
  lib,
  makeWrapper,
  gnuplot,
}: let
  cargoTOML = builtins.fromTOML (builtins.readFile ../Cargo.toml);
in
  rustPlatform.buildRustPackage {
    inherit (cargoTOML.package) version;

    pname = "calc";
    src = ../.;

    nativeBuildInputs = [makeWrapper];
    postFixup = ''
      wrapProgram $out/bin/mini-calc \
        --prefix PATH : "${lib.makeBinPath [gnuplot]}"
    '';

    cargoLock.lockFile = ../Cargo.lock;
    meta.mainProgram = "mini-calc";
  }
