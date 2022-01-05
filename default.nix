{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs { } }:
pkgs.haskell.lib.overrideCabal
  (pkgs.haskellPackages.callCabal2nix "nix-output-monitor" ./. { })
{ testTarget = "unit-tests"; }
