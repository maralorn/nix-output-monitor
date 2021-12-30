rec {
  remote = derivation {
    system = builtins.currentSystem;
    name = "remote-build";
    builder = ./golden1-builder.sh;
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
    input2 = remote;
    imput3 = local3;
    name = "local-build-2";
    builder = ./golden1-builder.sh;
    preferLocalBuild = true;
  };
  local3 = derivation {
    system = builtins.currentSystem;
    name = "local-build-3";
    builder = ./golden1-builder.sh;
    preferLocalBuild = true;
  };
}
