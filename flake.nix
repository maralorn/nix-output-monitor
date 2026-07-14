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
    let
      inherit (nixpkgs) lib;
    in
    {
      overlays.default = _: pkgs: {
        nix-output-monitor =
          let
            inherit (pkgs) haskellPackages haskell;
            inherit (pkgs.stdenv.hostPlatform) system;
            hlib = (_: haskell.lib.compose) system;
            integration-test-builds = import ./nix-output-monitor/test/integration/all.nix;
            doCheck = system == "x86_64-linux";
            injectChecks =
              opts:
              opts
              // {
                postInstall = opts.postInstall or "" + ''
                  mkdir -p $test
                  cp ./dist/build/integration-tests/integration-tests $test/integration-tests
                '';
                preCheck = ''
                  # Make sure integration-tests runtime and buildtime paths are available
                  # ${toString integration-test-builds}
                  # Other tests call nix, which we can’t do from within a nix build, so we disable it with this variable.
                  export TESTS_FROM_FILE=true;
                '';
              };
          in
          lib.pipe { } [
            (haskellPackages.callPackage ./nix-output-monitor)
            haskellPackages.buildFromCabalSdist
            hlib.justStaticExecutables
            (hlib.appendConfigureFlag "--ghc-option=-Werror --ghc-option=-Wno-error=unrecognised-warning-flags")

            (
              if doCheck then
                (
                  drv:
                  drv.overrideAttrs (oldAttrs: {
                    outputs = oldAttrs.outputs or [ ] ++ [ "test" ];
                  })
                )
              else
                lib.id
            )

            (hlib.overrideCabal (
              (if doCheck then injectChecks else lib.id) {
                inherit doCheck;
                buildTools = [ pkgs.installShellFiles ];
                postInstall = ''
                  ln -s nom "$out/bin/nom-build"
                  ln -s nom "$out/bin/nom-shell"
                  chmod a+x $out/bin/nom-shell
                  installShellCompletion completions/*
                '';
              }
            ))
          ];
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = lib.attrValues self.overlays;
        };
      in
      rec {
        packages = {
          generateReplayFiles = import ./generate-expected.nix { inherit system; } "stable";

          default = pkgs.nix-output-monitor;
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
              nixfmt = {
                enable = true;
                excludes = [ "^nix-output-monitor/default.nix" ];
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
              typos.enable = true;
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
              nom-test = lib.getOutput "test" packages.default;
              inherit system lib;
              inherit (pkgs.testers) runNixOSTest;
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
        devShells.default = pkgs.haskellPackages.shellFor {
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
