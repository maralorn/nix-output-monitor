{
  system,
  lib,
  nom-test,
}:

let
  pin = import ./test/golden/pin.nix;
  pkgs = import pin { inherit system; };
  all-golden-tests = import ./test/golden/all.nix;
in

nixPackage:
pkgs.testers.runNixOSTest {
  name = "nom_${lib.getName nixPackage}_${lib.getVersion nixPackage}";
  nodes.machine =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.busybox ];

      nix.nixPath = [ "nixpkgs=${pin}" ];

      nix = {
        package = nixPackage;

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

    machine.succeed("nix-store -r ${pin}")

    # golden tests path are hard coded in the test binary.
    machine.execute("cp -r ${nom-test}/golden-tests .")
    machine.execute("mkdir -p test")
    machine.execute("cp -r ${./test/golden} test/golden")
    exitcode, stdout = machine.execute("./golden-tests 2>&1 | tee stdout.log")
    print(exitcode)

    # ${toString (all-golden-tests ++ map (x: x.drvPath) all-golden-tests)}
    # ${pin}

    machine.copy_from_machine("stdout.log")
    machine.copy_from_machine("processing.log")
    machine.copy_from_machine("stdout-from-nix.log")
    machine.copy_from_machine("stderr-from-nix.log")
  '';
}
