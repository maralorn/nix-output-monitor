{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/7e53f5f41cdc9923fd8a62a877cc3165de02b19d";
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
        ghc-version = "92";
        inherit (nixpkgs.legacyPackages.${system}) lib haskell pkgs;
        haskellPackages = haskell.packages."ghc${ghc-version}";
        hlib = (_: haskell.lib.compose) system;
        inherit (hlib) doJailbreak dontCheck;
        golden-tests = import ./test/golden/all.nix;
        cleanSelf = lib.sourceFilesBySuffices self [
          ".hs"
          ".cabal"
          "stderr"
          "stdout"
          "stderr.json"
          "stdout.json"
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
                      # ${lib.concatStringsSep ", " (golden-tests ++ map (x: x.drvPath) golden-tests)}
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
            .overrideScope (
              if ghc-version == "94"
              then
                (final: prev: {
                  tasty-hedgehog = null;
                  doctest = null;
                  newtype-generics = doJailbreak prev.newtype-generics;
                  streamly = doJailbreak final.streamly_0_8_3;
                  optics = dontCheck prev.optics;
                  hermes-json = dontCheck (doJailbreak prev.hermes-json);
                  relude = dontCheck (doJailbreak final.relude_1_1_0_0);
                  unicode-data = dontCheck prev.unicode-data;
                })
              else _: _: {}
            );
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run rec {
            src = ./.;
            settings.ormolu.defaultExtensions = [
              "TypeApplications"
              "BangPatterns"
              "ImportQualifiedPost"
              "BlockArguments"
            ];
            hooks = {
              hlint.enable = true;
              alejandra.enable = true;
              nix-linter.enable = true;
              statix.enable = true;
              fourmolu = {
                enable = true;
                entry = lib.mkForce "${pkgs.haskell.packages.ghc92.fourmolu_0_9_0_0}/bin/fourmolu --mode inplace ${lib.escapeShellArgs (lib.concatMap (ext: ["--ghc-opt" "-X${ext}"]) settings.ormolu.defaultExtensions)}";
              };
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
            pkgs.haskell.packages.ghc92.weeder
            pkgs.haskellPackages.cabal-install
            pkgs.pv
          ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
