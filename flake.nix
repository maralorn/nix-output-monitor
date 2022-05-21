{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs @ {
    self,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (
      system: let
        inherit (inputs.nixpkgs.legacyPackages.${system}) lib haskell pkgs;
        haskellPackages = haskell.packages.ghc922;
        inherit (haskell.lib) doJailbreak dontCheck;
        golden-test = import ./test/golden1.nix {
          seed = "1";
          inherit system;
        };
      in rec
      {
        packages = {
          default =
            (haskell.lib.overrideCabal
            (haskellPackages.callCabal2nix "nix-output-monitor" ./. {})
            {
              preCheck = ''
                # ${lib.concatStringsSep ", " (lib.attrValues golden-test ++ map (x: x.drvPath) (lib.attrValues golden-test))}
                export TESTS_FROM_FILE=true;
              '';
            })
            .overrideScope (_: prev: {
              relude = haskell.lib.compose.appendPatch (pkgs.fetchpatch {
                url = "https://github.com/kowainik/relude/commit/d1fc18690b31b70ebbe68730270b73f53a5c7f12.patch";
                sha256 = "sha256-tsozpK/AkhUE3tgUqjyDK/tnrujNd7yJdbLIUFZvJHk=";
              }) (doJailbreak (dontCheck prev.relude));
              optics = dontCheck prev.optics;
            });
        };
        checks = {
          pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
            src = ./.;
            settings.ormolu.defaultExtensions = [
              "TypeApplications"
              "BangPatterns"
              "ImportQualifiedPost"
            ];
            hooks = {
              hlint.enable = true;
              alejandra.enable = true;
              nix-linter.enable = true;
              statix.enable = true;
              #fourmolu.enable = true; # disabled until fourmolo or ormolu can deal with RecordDotSyntax
              cabal-fmt.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        devShell = haskellPackages.shellFor {
          packages = _: [packages.default];
          buildInputs = [
            inputs.pre-commit-hooks.defaultPackage.${system}
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            pkgs.expect
          ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
