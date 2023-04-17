{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        ghc-version = "94";
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
          "default.nix"
        ];
      in rec {
        packages = {
          default =
            (lib.pipe {}
              [
                (haskellPackages.callPackage self)
                haskellPackages.buildFromCabalSdist
                hlib.justStaticExecutables
                (hlib.overrideCabal
                  {
                    src = cleanSelf;
                    preConfigure = ''
                      echo "Checking that default.nix is up-to-date."
                      ${haskellPackages.cabal2nix}/bin/cabal2nix . > fresh-default.nix
                      cp ${cleanSelf}/default.nix .
                      ${pkgs.alejandra}/bin/alejandra -q fresh-default.nix default.nix
                      ${pkgs.diffutils}/bin/diff -w default.nix fresh-default.nix
                    '';
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
                  streamly = doJailbreak final.streamly_0_8_3;
                  optics = dontCheck prev.optics;
                  hermes-json = dontCheck (doJailbreak prev.hermes-json);
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
              #nix-linter.enable = true;
              statix.enable = true;
              fourmolu = {
                enable = true;
                entry = lib.mkForce "${pkgs.haskellPackages.fourmolu}/bin/fourmolu --mode inplace ${lib.escapeShellArgs (lib.concatMap (ext: ["--ghc-opt" "-X${ext}"]) settings.ormolu.defaultExtensions)}";
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
