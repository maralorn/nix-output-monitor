nix-output-monitor
==================

Pipe your nix-build output through the nix-output-monitor (aka nom) to get additional information while building.

## Status

This is an experimental fun project. The purpose of it is to write something fun and useful in Haskell. You are free and very welcome to contribute feedback, issues or PRs, but I do not commit to maintain this project over a long time period.
This program relies on the output of the v1 nix commands (i.e. nix-build not nix build) and will hopefully one day be obsolete.

Best case scenario: This could serve as inspiration as to how to improve nix output in the future.

## Installing

* cabal: Install `cabal-install` and run `cabal install` in the checked out repo.
* nix: or run `nix-build` or `nix-env` or include the `default.nix` of this repo in your nixos config.

## Running

Once you have installed `nix-output-monitor` to your path, run any nix command (`nixos-rebuild`,`nix-build`,`home-manager switch`, **not** `nix build`.) and pipe stderr and stdout into `nom`.

```
nix-build |& nom
```

**Don‘t forget to redirect stderr, too.** That's what the `&`, does.

## Icons

```Haskell
running = "▶" -- yellow
done = "✔" -- green
todo = "⏳" -- blue
```

If you can‘t see all icons you maybe need another terminal font.
I recommend any font from `pkgs.nerdfonts` e.g. `"JetBrainsMono Nerd Font"`.

## Example Run

With some local builds and downloads:

[![asciicast](https://asciinema.org/a/HKWeTpFS42muAaJapSvMiSEbn.svg)](https://asciinema.org/a/HKWeTpFS42muAaJapSvMiSEbn)

With more builds on a remote server:

[![asciicast](https://asciinema.org/a/1TVTTlogGdmbC1jtwWCPiatb7.svg)](https://asciinema.org/a/1TVTTlogGdmbC1jtwWCPiatb7)

## Implementation

The program consists of three steps in a loop, powered by Haskell lazyness.

1. parsing (If you have suggestion for improving my parser combinators, I am all ears!)
2. updating state (with IO to look into the the nix store.)
3. printing

Right now it uses three sources of information:

1. The parsed nix-build output
2. it checks if build results exist in the nix-store
3. it querys `.drv` files for information about the `out` output path.

## Caveats

This will fail in unexpected and expected ways.
nix-output-monitor receives most it's information from parsing nix-build output. The parser might be to strict or to loose for use cases I didn‘t think of. Then **the numbers displayed will be off**!

Terminal clearing and reprinting is brittle. It might fail with your terminal or terminal width.
This program also makes assumptions like your nix-store is at "/nix/store" or that every derivation has an output at "out".

The formatting code is a mess and has no tests, so feel free to tell me about any corner cases where it breaks.

Luckily I don‘t think this program screws up anything more than your terminal.

## Strengths

The biggest strength of nom are ovbiously the colorful unicode symbols! (If you don‘t agree I accept PRs to making that configurable.)

nom ignores any output it doesn‘t recognize and will always print out everything it receives. So you can never loose information (besides coloring of nix output).

nom does not assume that you run exactly one nix-build. If you run e.g. a script running multiple builds it will aggregate the information of all of them.
