{ seed }:
let
  mkBuild = name: deps: script:
    import ./.. {
      inherit name deps seed script;
      busybox = true;
    };
in rec {
  build_long = mkBuild "build-long" [ ] "$sleep 10s";
  build_waiting = mkBuild "build-waiting" [ build_long build_fail ] "";
  build_fail = mkBuild "build-fail" [ ] "$sleep 1s; exit 1;";
}
