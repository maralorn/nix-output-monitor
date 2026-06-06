args@{
  busybox ? false,
  ...
}:
let
  nixpkgs = import ./pin.nix;
  pkgs = import nixpkgs { };
  inherit (pkgs) lib;

  busyboxBin = lib.makeBinPath [ pkgs.busybox ];
in
if busybox then
  derivation (
    args
    // {
      system = builtins.currentSystem;
      builder = busyboxBin + "/sh";
      sleep = busyboxBin + "/sleep";
      args = [
        "-c"
        args.script
      ];
    }
  )
else
  derivation (
    args
    // {
      system = "x86_64-linux";
      builder = "/bin/sh";
      args = [
        "-c"
        args.script
      ];
    }
  )
