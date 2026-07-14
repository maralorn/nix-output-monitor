let
  packages =
    path:
    let
      withSeed = seed: builtins.attrValues (import path { inherit seed; });
    in
    withSeed "old-style" ++ withSeed "json";
  standard-builds = packages ./standard;
  fail-builds = packages ./fail;
in
standard-builds
++
  # Use unsafeDiscardOutputDependency to introduce a dependency on the derivation path but not its outputs.
  map (x: builtins.unsafeDiscardOutputDependency x.drvPath) (fail-builds ++ standard-builds)
