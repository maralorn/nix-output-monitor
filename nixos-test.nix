{
  runNixOSTest,
  system,
  lib,
  nom-test,
}:

let
  pin = import ./test/integration/pin.nix;
  pinned-pkgs = import pin { inherit system; };
  integration-test-builds = import ./test/integration/all.nix;
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
        # When building a, b, c where a depends on b, c should build instead of being pending.
        # For that reason, we need 4 jobs (4 > 3 which are the a, b, and c builds).
        max-jobs = 4;
      };
    };
  };

  testScript = /* python */ ''
    start_all()

    # (with_nix) Ensure the nixpkgs to evaluate is available
    machine.succeed("nix-store -r ${pin}")
    # (from files) Make sure integration-tests runtime and buildtime paths are available
    # ${toString integration-test-builds}

    # integration tests path are hard coded in the test binary.
    machine.execute("cp -r ${nom-test}/integration-tests .")
    machine.execute("mkdir -p test")
    machine.execute("cp -r ${./test/integration} test/integration")
    machine.succeed("./integration-tests 2>&1 | tee stdout.log")
  '';
}
