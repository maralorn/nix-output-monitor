# A floating content-addressed derivation. Building this live requires the
# ca-derivations experimental feature, so the integration test replays the
# recorded log (stderr.json) instead of running nix — the recording stems
# from a real 16-builder run and contains the "resolved derivation" activity
# (type 111) that ties the announced derivation to its resolved twin.
{seed}:
derivation {
  name = "ca-test";
  system = builtins.currentSystem;
  builder = "/bin/sh";
  args = ["-c" "echo ${seed} > $out"];
  __contentAddressed = true;
  outputHashMode = "recursive";
  outputHashAlgo = "sha256";
}
