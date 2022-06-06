{ pkgs ? import <nixpkgs> {} }:
let clay = pkgs.haskellPackages.callHackage "clay" "0.13.3" {};
in  pkgs.haskellPackages.callCabal2nix "static-css" ./. { inherit clay; }
