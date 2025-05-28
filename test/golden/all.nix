let
  packages = seed: (import ./standard { inherit seed; });
in
builtins.attrValues (packages "old-style") ++ builtins.attrValues (packages "json")
