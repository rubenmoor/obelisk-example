{ pkgs ? import <nixpkgs> {} }:
let
  staticApp = import ./static-app/release.nix {};
in pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./static;
  buildInputs = [staticApp];
  installPhase = ''
    mkdir -p $out
    ${staticApp}/bin/static-app > $out/styles.css
    cp -r * $out/
  '';
}
