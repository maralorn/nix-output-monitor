{
  remote = derivation {
    system = builtins.currentSystem;
    name = "remote-build";
    builder = ./golden1-builder.sh;
  };
  local = derivation {
    system = builtins.currentSystem;
    name = "local-build";
    builder = ./golden1-builder.sh;
    preferLocalBuild = true;
  };
}
