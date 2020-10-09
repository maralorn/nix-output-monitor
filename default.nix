{ pkgs ? import <nixpkgs> { } }:

# Generate cabal2nix.nix with cabal2nix.

pkgs.haskell.lib.overrideCabal
(pkgs.haskellPackages.callCabal2nix "nix-output-monitor" ./. { }) {
  # Can't ran the golden-tests with nix, because they call nix
  testTarget = "unit-tests";
  src = fetchFromGitHub {
    owner = "maralorn";
    repo = "nix-output-monitor";
    sha256 = "0000000000000000000000000000000000000000000000000000";
    rev = "a0e0b09";
  };
}
