let
  packages = seed: (import ./standard {inherit seed;});
in
  packages "old-style" // packages "json"
