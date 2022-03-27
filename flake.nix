{
  description = "nix-output-monitor";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
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
        inherit (inputs.nixpkgs.legacyPackages.${system}) lib haskellPackages haskell pkgs;
        golden-test = import ./test/golden1.nix {
          seed = "1";
          inherit system;
        };
      in rec
      {
        packages = {
          default =
            haskell.lib.overrideCabal
            (haskellPackages.callCabal2nix "nix-output-monitor" ./. {})
            {
              preCheck = ''
                # ${lib.concatStringsSep ", " ((lib.attrValues golden-test) ++ map (x: x.drvPath) (lib.attrValues golden-test))}
                export TESTS_FROM_FILE=true;
                exit 1
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
              alejandra.enable = true;
              nix-linter.enable = true;
              statix.enable = true;
              fourmolu.enable = true;
              cabal-fmt.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        devShell = haskellPackages.shellFor {
          packages = _: [packages.default];
          buildInputs = [inputs.pre-commit-hooks.defaultPackage.${system} haskellPackages.haskell-language-server pkgs.expect];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
