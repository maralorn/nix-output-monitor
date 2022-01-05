{ seed }:
let
  mkBuild = name: deps: derivation {
    system = builtins.currentSystem;
    inherit name deps seed;
    builder = ./golden1-builder.sh;
  };
in
rec {
  build1 = mkBuild "build1" [ ];
  build2 = mkBuild "build2" [ build1 ];
  build3 = mkBuild "build3" [ build1 build2 ];
  build4 = mkBuild "build4" [ build2 build3 ];
}
