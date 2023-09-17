{ seed }:
let
  mkBuild = name: deps:
    import ./.. {
      inherit name deps seed;
      script = "echo output for $out; echo foo > $out";
    };
in rec {
  build1 = mkBuild "build1" [ ];
  build2 = mkBuild "build2" [ build1 ];
  build3 = mkBuild "build3" [ build1 build2 ];
  build4 = mkBuild "build4" [ build2 build3 ];
}
