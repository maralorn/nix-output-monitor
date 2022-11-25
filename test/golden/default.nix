args @ {busybox ? false, ...}: let
  busyboxPath = "/nix/store/y7ji7mwys7g60j2w8bl93cmfbvd3xi3r-busybox-static-x86_64-unknown-linux-musl-1.35.0/bin/";
  busyboxBin = builtins.storePath busyboxPath;
in
  if busybox
  then
    derivation (args
      // {
        system = builtins.currentSystem;
        builder = busyboxBin + "/sh";
        sleep = busyboxBin + "/sleep";
        args = ["-c" args.script];
      })
  else
    derivation (args
      // {
        system = "x86_64-linux";
        builder = "/bin/sh";
        args = ["-c" args.script];
      })
