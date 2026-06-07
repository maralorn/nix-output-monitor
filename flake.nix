{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-hooks,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        inherit (nixpkgs.legacyPackages.${system})
          lib
          haskell
          pkgs
          haskellPackages
          ;
        hlib = (_: haskell.lib.compose) system;
        golden-tests = import ./test/golden/all.nix;
        cleanSelf = lib.sourceFilesBySuffices self [
          ".hs"
          ".cabal"
          "stderr"
          "stdout"
          "stderr.json"
          "stdout.json"
          ".zsh"
          ".bash"
          ".fish"
          "LICENSE"
          "CHANGELOG.md"
          "default.nix"
        ];
      in
      rec {
        packages = {
          generateGoldenFiles = import ./generate-expected.nix { inherit system; } "nix_2_28";

          default = lib.pipe { } [
            (haskellPackages.callPackage cleanSelf)
            haskellPackages.buildFromCabalSdist
            hlib.justStaticExecutables
            (hlib.appendConfigureFlag "--ghc-option=-Werror --ghc-option=-Wno-error=unrecognised-warning-flags")

            (
              drv:
              drv.overrideAttrs (oldAttrs: {
                outputs = oldAttrs.outputs or [ ] ++ [ "test" ];
              })
            )

            (hlib.overrideCabal (
              {
                doCheck = false;
                buildTools = [ pkgs.installShellFiles ];
                postInstall = ''
                  ln -s nom "$out/bin/nom-build"
                  ln -s nom "$out/bin/nom-shell"
                  chmod a+x $out/bin/nom-shell
                  installShellCompletion completions/*
                '';
              }
              // lib.optionalAttrs (system == "x86_64-linux") {
                doCheck = true;
                preCheck =
                  let
                    pin = import ./test/golden/pin.nix;
                    pkgs = import pin { inherit system; };
                  in
                  ''
                    # Make sure some paths are available
                    # ${lib.concatStringsSep ", " (golden-tests ++ map (x: x.drvPath) golden-tests)}
                    # ${pkgs.busybox}
                    export TESTS_FROM_FILE=true;
                  '';
                postInstall = ''
                  mkdir -p $test
                  cp ./dist/build/golden-tests/golden-tests $test/golden-tests
                '';
              }
            ))
          ];
        };
        checks = {
          git-hooks-check = git-hooks.lib.${system}.run {
            src = ./.;
            tools = {
              fourmolu = lib.mkForce (lib.getBin pkgs.haskellPackages.fourmolu);
              cabal-gild = lib.mkForce (lib.getBin pkgs.haskellPackages.cabal-gild);
            };
            default_stages = [
              "manual"
              "pre-push"
            ];
            hooks = {
              hlint.enable = true;
              nixfmt-rfc-style = {
                enable = true;
                excludes = [ "^default.nix" ];
              };
              cabal2nix.enable = true;
              nil.enable = true;
              editorconfig-checker = {
                enable = true;
                excludes = [ ".*\\.md" ];
              };
              deadnix = {
                enable = true;
                excludes = [ "default.nix" ];
              };
              statix.enable = true;
              fourmolu.enable = true;
              ormolu.settings.defaultExtensions = [
                "TypeApplications"
                "BangPatterns"
                "ImportQualifiedPost"
                "BlockArguments"
              ];
              shellcheck = {
                enable = true;
                excludes = [ "\\.zsh" ];
              };
              cabal-gild.enable = true;
            };
          };
        }
        // (
          let
            nixPackages =
              map (nixVersion: pkgs.nixVersions.${nixVersion}) [
                "stable"
                "latest"
                "git"
              ]
              ++ map (lixVersion: pkgs.lixPackageSets.${lixVersion}.lix) [
                "stable"
                "latest"
                "git"
              ];
            testNomAgainstNixImpl = import ./nixos-test.nix {
              nom-test = packages.default;
              inherit system lib;
            };
            allTests = lib.genAttrs' nixPackages (
              nixImpl:
              lib.nameValuePair "nixos-test_${lib.getName nixImpl}_${lib.replaceString "." "_" (lib.getVersion nixImpl)}" (
                testNomAgainstNixImpl nixImpl
              )
            );
          in
          allTests
          // {
            nixos-test_all = pkgs.runCommand "nixos-test" { } ''
              # ${toString (lib.attrValues allTests)}
              touch $out
            '';
          }
        );
        devShells.default = haskellPackages.shellFor {
          packages = _: [ packages.default ];
          buildInputs = [
            (lib.getBin pkgs.haskellPackages.fourmolu)
            git-hooks.packages.${system}.default
            pkgs.haskell-language-server
            pkgs.hlint
            (lib.getBin pkgs.haskellPackages.weeder)
            pkgs.cabal-install
            pkgs.pv
          ];
          withHoogle = true;
          inherit (self.checks.${system}.git-hooks-check) shellHook;
        };
      }
    );
}
