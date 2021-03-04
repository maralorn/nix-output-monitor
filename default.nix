{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs {} }:
let
  golden-test = import ./test/golden1.nix;
in
pkgs.haskell.lib.overrideCabal
  (pkgs.haskellPackages.callCabal2nix "nix-output-monitor" ./. {})
  {
    preCheck = "# ${golden-test.local} ${golden-test.remote} ${golden-test.local.drvPath} ${golden-test.remote.drvPath}";
  }
