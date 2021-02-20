{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.lib.overrideCabal
  (pkgs.haskellPackages.callCabal2nix "nix-output-monitor" ./. {}) {
  # Can't ran the golden-tests with nix, because they call nix
  testTarget = "unit-tests";
}
