{
  runNixOSTest,
  system,
  lib,
  nom-test,
}:

let
  pin = import ./test/golden/pin.nix;
  pinned-pkgs = import pin { inherit system; };
  all-golden-tests = import ./test/golden/all.nix;
in

nixPackage:
runNixOSTest {
  name = "nom_${lib.getName nixPackage}_${lib.getVersion nixPackage}";
  nodes.machine = {
    # "with_nix" tests builds with minimal path requirements (just busybox).
    # Removing this will fail because qemu can't access internet.
    environment.systemPackages = [ pinned-pkgs.busybox ];
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

    # (with_nix) Ensure the nixpkgs to evaluate is available
    machine.succeed("nix-store -r ${pin}")
    # (from files) Make sure golden-tests runtime and buildtime paths are available
    # ${toString all-golden-tests}

    # golden tests path are hard coded in the test binary.
    machine.execute("cp -r ${nom-test}/golden-tests .")
    machine.execute("mkdir -p test")
    machine.execute("cp -r ${./test/golden} test/golden")
    machine.succeed("./golden-tests 2>&1 | tee stdout.log")
  '';
}
