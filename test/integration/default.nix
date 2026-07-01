args@{
  busybox ? false,
  ...
}:
let
  nixpkgs = import ./pin.nix;
  pkgs = import nixpkgs { system = "x86_64-linux"; };
  busyBoxExe = pkgs.lib.getExe' pkgs.busybox;
in
if busybox then
  derivation (
    args
    // {
      system = "x86_64-linux";
      builder = busyBoxExe "sh";
      sleep = busyBoxExe "sleep";
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
