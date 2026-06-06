{ system }:

let
  pin = import ./test/golden/pin.nix;
  pkgs = import pin { inherit system; };
in

nixVersion:
pkgs.testers.runNixOSTest {
  name = "nom_${nixVersion}";
  nodes.machine =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.busybox ];

      nix = {
        package = pkgs.nixVersions.${nixVersion};

        nixPath = [ "nixpkgs=${pin}" ];

        settings = {
          substitute = false;

          # nom testsuite uses nix-command, probably flakes
          experimental-features = [
            "nix-command"
            "flakes"
          ];

          # When building a, b, c where a depends on b, c should build instead of being pending to match the golden files.
          max-jobs = 4;
        };
      };
    };

  testScript = /* python */ ''
    start_all()
    machine.execute("cp -r ${./test/golden}/* .")
    machine.succeed("nix-build ./standard/default.nix --no-out-link --argstr seed \"old-style\" 2>./standard/stderr 1>./standard/stdout")
    machine.succeed("nix-build ./standard/default.nix --no-out-link --argstr seed \"json\" --log-format internal-json -v 2>./standard/stderr.json 1>./standard/stdout.json")
    machine.fail("nix-build ./fail/default.nix --no-out-link --argstr seed \"old-style\" 2>./fail/stderr 1>./fail/stdout")
    machine.fail("nix-build ./fail/default.nix --no-out-link --argstr seed \"json\" --log-format internal-json -v 2>./fail/stderr.json 1>./fail/stdout.json")

    machine.copy_from_machine("./standard")
    machine.copy_from_machine("./fail")
  '';
}
