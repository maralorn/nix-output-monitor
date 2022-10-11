{
  description = "nix-output-monitor";
  inputs = {
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (
      system: let
        inherit (nixpkgs.legacyPackages.${system}) lib haskell pkgs;
        haskellPackages = haskell.packages.ghc92;
        hlib = haskell.lib.compose;
        inherit (hlib) doJailbreak dontCheck;
        golden-test = import ./test/golden1.nix {
          seed = "1";
          inherit system;
        };
        cleanSelf = lib.sourceFilesBySuffices self [
          ".hs"
          ".cabal"
          ".stderr"
          ".stdout"
          ".zsh"
          "LICENSE"
          "CHANGELOG.md"
        ];
      in rec {
        packages = {
          default =
            (lib.pipe {}
              [
                (haskellPackages.callCabal2nix "nix-output-monitor" cleanSelf)
                haskellPackages.buildFromCabalSdist
                hlib.justStaticExecutables
                (hlib.overrideCabal
                  {
                    preCheck = ''
                      # ${lib.concatStringsSep ", " (lib.attrValues golden-test ++ map (x: x.drvPath) (lib.attrValues golden-test))}
                      export TESTS_FROM_FILE=true;
                    '';
                    buildTools = [pkgs.installShellFiles];
                    postInstall = ''
                      ln -s nom "$out/bin/nom-build"
                      ln -s nom "$out/bin/nom-shell"
                      chmod a+x $out/bin/nom-shell
                      installShellCompletion --zsh --name _nom-build completions/completion.zsh
                    '';
                  })
              ])
            .overrideScope (_: prev: {
              relude = hlib.appendPatch (pkgs.fetchpatch {
                url = "https://github.com/kowainik/relude/commit/d1fc18690b31b70ebbe68730270b73f53a5c7f12.patch";
                sha256 = "sha256-tsozpK/AkhUE3tgUqjyDK/tnrujNd7yJdbLIUFZvJHk=";
              }) (doJailbreak (dontCheck prev.relude));
              optics = dontCheck prev.optics;
              hermes-json = doJailbreak prev.hermes-json;
            });
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
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
              # fourmolu.enable = true; # disabled until fourmolo or ormolu can deal with RecordDotSyntax
              cabal-fmt.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        devShells.default = haskellPackages.shellFor {
          packages = _: [packages.default];
          buildInputs = [
            pre-commit-hooks.defaultPackage.${system}
            haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hs-speedscope
            pkgs.pv
          ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
