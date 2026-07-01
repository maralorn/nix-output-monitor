# This is the nixpkgs used for integration tests
# Changing this means rotating the integration test replay files

builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/331800de5053fcebacf6813adb5db9c9dca22a0c.tar.gz";
  sha256 = "x5UQuRsH3MqI0U9afaXSNqzTPSeZlRLvFAav2Ux1pNw=";
}
