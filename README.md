nix-output-monitor
==================

Pipe your nix-build output through the nix-output-monitor to get additional information while building.

## Status

This is an experimental fun project. You are free and very welcome to contribute feedback, issues or PRs, but I do not commit to maintain this project over a long time period.
This program relies on the output of the v1 nix commands (i.e. nix-build not nix build) and will hopefully one day be obsolete.

Best case scenario: This could serve as inspiration as to how to improve nix output in the future.

## Installing

* cabal: Install `cabal-install` and run `cabal install` in the checked out repo.
* nix: or run `nix-build` or `nix-env` or include the `default.nix` of this repo in your nixos config.

## Running

Once you have installed `nix-output-monitor` to your path, run any nix command and pipe stderr and stdout into it.

```
nix-build 2>&1 | nom
```

## Caveats

This will fail in unexpected and expected ways.
nix-output-monitor receives most it's information from parsing nix-build output. The parser might be to strict or to loose for use cases I didn‘t think of. Then *the numbers display will be off*!

Terminal clearing and reprinting is brittle. It might fail with your terminal or terminal width.
This program also makes assumptions like your nix-store is at "/nix/store" or that every derivation has an output at "out".

## Strengths

nom ignores any output it doesn‘t recognize and will always print out everything it receives so you can never loose information. (besides coloring of nix output).
nom does not assume that you run exaclty one nix-build. If you run e.g. a script running multiple builds it will aggregate the information of all of them.
