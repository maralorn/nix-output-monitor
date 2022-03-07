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
  builda1 = mkBuild "builda1" [ ];
  builda2 = mkBuild "builda2" [ builda1 ];
  builda3 = mkBuild "builda3" [ builda1 builda2 ];
  builda4 = mkBuild "builda4" [ builda2 builda3 ];
  build5 = mkBuild "build5" [ build4 builda4 ];
}
