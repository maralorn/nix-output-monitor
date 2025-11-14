{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    git-hooks = {
      url = "github:cachix/git-hooks.nix/31792452cf92d204ea0df8e3fddc018235c4cf1b";
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
          default = lib.pipe { } [
            (haskellPackages.callPackage self)
            haskellPackages.buildFromCabalSdist
            hlib.justStaticExecutables
            (hlib.appendConfigureFlag "--ghc-option=-Werror --ghc-option=-Wno-error=unrecognised-warning-flags")

            (hlib.overrideCabal (
              {
                src = cleanSelf;
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
                preCheck = ''
                  # ${lib.concatStringsSep ", " (golden-tests ++ map (x: x.drvPath) golden-tests)}
                  export TESTS_FROM_FILE=true;
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
            hooks = {
              hlint.enable = true;
              nixfmt-rfc-style = {
                excludes = [ "default.nix" ];
                enable = true;
              };
              cabal2nix.enable = true;
              nil.enable = true;
              editorconfig-checker = {
                excludes = [ ".*\\.md" ];
                enable = true;
              };
              deadnix.enable = true;
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
        };
        devShells.default = haskellPackages.shellFor {
          packages = _: [ packages.default ];
          buildInputs = [
            (lib.getBin pkgs.haskellPackages.fourmolu)
            git-hooks.packages.${system}.default
            pkgs.haskell-language-server
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
