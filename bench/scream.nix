{pkgs ? (import <nixpkgs> {})}:
pkgs.stdenv.mkDerivation {
  name = "robot-that-screams";
  buildCommand = ''
    for _ in {0..1000000}; do
      echo AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    done
  '';
}
