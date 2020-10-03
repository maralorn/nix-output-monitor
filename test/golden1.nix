{ pkgs ? import <nixpkgs> { } }:
let
  pinnedPkgs = import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/a4e6f8b134efc55aa2613109548de2042a32d0bb.tar.gz";
    sha256 = "0g3sgkjcvhv56310vbswcnl2xcdfa2nasa951lvzb9ypvfpb69r7";
  }) { };
in {
  remote = pinnedPkgs.writers.writeHaskell "hello" { }
    ''main = putStrLn "Hello World!"'';
  local = pinnedPkgs.writeText "hello" "Hello World!";
}
