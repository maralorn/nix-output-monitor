{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs { } }:
let
  inherit (pkgs) lib;
  golden-test = import ./test/golden1.nix { seed = "1"; };
in
pkgs.haskell.lib.overrideCabal
  (pkgs.haskellPackages.callCabal2nix "nix-output-monitor" ./. {})
  {
    preCheck = ''
      # ${lib.concatStringsSep ", " ((lib.attrValues golden-test) ++ map (x: x.drvPath) (lib.attrValues golden-test))}
      export TESTS_FROM_FILE=true;
      '';
  }
