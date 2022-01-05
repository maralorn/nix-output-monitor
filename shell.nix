{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs { } }:
pkgs.haskellPackages.shellFor {
  packages = p: [ (import ./default.nix { inherit sources pkgs; }) ];
}
