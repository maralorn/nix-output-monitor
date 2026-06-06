{
  system,
  lib,
  nom-test,
}:

let
  pin = import ./test/golden/pin.nix;
  pkgs = import pin { inherit system; };
in

nixPackage:
pkgs.testers.runNixOSTest {
  name = "nom_${lib.getName nixPackage}_${lib.getVersion nixPackage}";
  nodes.machine =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.busybox ];

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

    # golden tests path are hard coded in the test binary.
    machine.execute("cp -r ${nom-test}/golden-tests .")
    machine.execute("cp -r ${./test/golden}/* .")
    exitcode, stdout = machine.execute("./golden-tests 2>&1 > stdout.log")
  '';
}
