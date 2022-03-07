{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib haskellPackages haskell;
        golden-test = import ./test/golden1.nix { seed = "1"; inherit system; };
      in
      rec
      {
        defaultPackage = packages.nix-output-monitor;
        packages = {
          nix-output-monitor =
            haskell.lib.overrideCabal
              (haskellPackages.callCabal2nix "nix-output-monitor" ./. { })
              {
                preCheck = ''
                  # ${lib.concatStringsSep ", " ((lib.attrValues golden-test) ++ map (x: x.drvPath) (lib.attrValues golden-test))}
                  export TESTS_FROM_FILE=true;
                '';
              };
        };
        checks = {
          pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
            src = ./.;
            settings.ormolu.defaultExtensions = [
              "TypeApplications"
              "BangPatterns"
            ];
            hooks = {
              hlint.enable = true;
              nixpkgs-fmt.enable = true;
              fourmolu.enable = true;
            };
          };
        };
        devShell = haskellPackages.shellFor {
          packages = p: [ defaultPackage ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
