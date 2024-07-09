{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "nixpkgs/haskell-updates";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      pre-commit-hooks,
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
          "LICENSE"
          "CHANGELOG.md"
          "default.nix"
        ];
      in
      rec {
        packages = {
          default = lib.pipe { } [
            (haskellPackages.callPackage self)
            haskellPackages.buildFromCabalSdist
            hlib.justStaticExecutables
            (hlib.appendConfigureFlag "--ghc-option=-Werror")
            (hlib.overrideCabal {
              src = cleanSelf;
              doCheck = system == "x86_64-linux";
              preCheck = ''
                # ${lib.concatStringsSep ", " (golden-tests ++ map (x: x.drvPath) golden-tests)}
                export TESTS_FROM_FILE=true;
              '';
              buildTools = [ pkgs.installShellFiles ];
              postInstall = ''
                ln -s nom "$out/bin/nom-build"
                ln -s nom "$out/bin/nom-shell"
                chmod a+x $out/bin/nom-shell
                installShellCompletion --zsh --name _nom-build completions/completion.zsh
              '';
            })
          ];
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
              nixfmt = {
                enable = true;
                excludes = [ "default.nix" ];
              };
              statix.enable = true;
              cabal2nix.enable = true;
              fourmolu = {
                enable = true;
                entry = lib.mkForce "${pkgs.haskellPackages.fourmolu}/bin/fourmolu --mode inplace ${
                  lib.escapeShellArgs (
                    lib.concatMap (ext: [
                      "--ghc-opt"
                      "-X${ext}"
                    ]) settings.ormolu.defaultExtensions
                  )
                }";
              };
              cabal-fmt.enable = true;
              shellcheck = {
                enable = true;
                excludes = [ "\\.zsh" ];
              };
            };
          };
        };
        devShells.default = haskellPackages.shellFor {
          packages = _: [ packages.default ];
          buildInputs = [
            pre-commit-hooks.packages.${system}.default
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
