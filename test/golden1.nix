rec {
  remote = derivation {
    system = builtins.currentSystem;
    name = "remote-build";
    builder = ./golden1-builder.sh;
    preferLocalBuild = true;
  };
  local = derivation {
    system = builtins.currentSystem;
    input = remote;
    name = "local-build";
    builder = ./golden1-builder.sh;
    preferLocalBuild = true;
  };
  local2 = derivation {
    system = builtins.currentSystem;
    input = local;
    name = "local-build-2";
    builder = ./golden1-builder.sh;
    preferLocalBuild = true;
  };
}
