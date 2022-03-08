# nix-output-monitor

Pipe your nix-build output through the nix-output-monitor (aka nom) to get additional information while building.

While your build runes nom will draw something like this at the bottom of your build log:

![](example-screenshot.png)

[![Packaging status](https://repology.org/badge/vertical-allrepos/nix-output-monitor.svg)](https://repology.org/project/nix-output-monitor/versions)

## Status

This was an experimental fun project, which proofed to be useful to quite a lot of people.
The purpose of it is to write something fun and useful in Haskell.
You are free and very welcome to contribute feedback, issues or PRs, but I do not commit to maintain this project over a long time period.
This program relies on the output of the v1 nix commands (i.e. `nix-build` and not `nix build`) support for v2 output may come in the future.

## Installing

* nixpkgs: nom is in nixpkgs/nixos unstable just install `pkgs.nix-output-monitor` in the usual way.
* cabal: Install `cabal-install` and run `cabal install` in the checked out repo.
* nix: or run `nix-build` or `nix-env` or include the `default.nix` of this repo in your nixos config.

## Running

### The Easy Way

When installed from nixpkgs you can replace every call to `nix-build` with the bundled wrapper script `nom-build`.

### The Flexible Way

Once you have installed `nix-output-monitor` to your path, run any nix command (`nixos-rebuild`,`nix-build`,`home-manager switch`, **not** `nix build`.) and pipe stderr and stdout into `nom`.

```shell
nix-build |& nom
```

**Don‘t forget to redirect stderr, too.** That's what the `&`, does.

### Preserving Colored Text

To preserve the color of the redirected text you can use the `unbuffer` command from the `expect` package. (The `nom-build` wrapper does this for you.)

```shell
unbuffer nix-build |& nom
```

## Explanation

### Legend

Nom tries to convey informations via symbols and colors

* ▶, yellow: running builds
* ✔, green: completed builds
* ⏳︎︎︎, blue: planned builds
* ⬇, cyan: downloads often in the form `completed/total`
* ⬆, magenta: uploads
* ⚠, red: failed builds
* ∅: a moving average over past builds of this derivation
* ⏱︎: running time 
* ∑: a summary over all packages and hosts

If you can‘t see all icons you maybe need another terminal font.
I recommend any font from `pkgs.nerdfonts` e.g. `"JetBrainsMono Nerd Font"`.
Also different terminals might work differently well. I recommend: `pkgs.foot`.

### How to Read the Dependency Graph

* nom will sort all builds into a tree.
* Children of a node are direct dependencies.
* nom will try to show you the most relevant part of the dependency tree, roughly aiming to fill a third of your terminal
* No build will be printed twice in the tree, it will only be shown for the lower most dependency.
* Everytime nom decides to not show all direct dependencies of a build (and for root nodes), it will print a `&` and a summary over the build state of all dependencies.
* Use the colors from above to read the summary

## Example Runs

An example remote build:
[![asciicast](https://asciinema.org/a/TASdstyOJm3reqFcKZrekgH65.svg)](https://asciinema.org/a/TASdstyOJm3reqFcKZrekgH65)

An example failing remote build:
[![asciicast](https://asciinema.org/a/TASdstyOJm3reqFcKZrekgH65.svg)](https://asciinema.org/a/TASdstyOJm3reqFcKZrekgH65)

An example really large build (`haskell-language-server`):
[![asciicast](https://asciinema.org/a/DDdRLAaiL65PsYUS4dvEaFWBm.svg)](https://asciinema.org/a/DDdRLAaiL65PsYUS4dvEaFWBm)

An example running `sudo nixos-rebuild switch`:
[![asciicast](https://asciinema.org/a/fQTfaxCjNQoNz9eJYGGrLZTcw.svg)](https://asciinema.org/a/fQTfaxCjNQoNz9eJYGGrLZTcw)

## Implementation

Right now nom uses four sources of information:

1. The parsed nix-build output
2. it checks if build results exist in the nix-store
3. it querys `.drv` files for information about the `out` output path.
4. It caches build times in `$XDG_CACHE_HOME/nix-output-monitor/build-reports.csv`.

## Limitations

* This will fail in unexpected and expected ways.
* nix-output-monitor receives most it's information from parsing nix-build output. The parser might be to strict or to loose for use cases I didn‘t think of. Then **the numbers displayed will be off**!
* nix-build does not show info when a download or upload is finished, so we currently cannot differentiate between started and completed downloads.
* remote builds will sometimes be shown as running even when they are actually still waiting for uploads or downloads.

* Terminal clearing and reprinting is brittle. It might fail with your terminal or terminal width. But at this point I‘ve invested some effort to make it usable.
* This program also makes assumptions like your nix-store is at "/nix/store" or that every derivation has an output at "out".

* Luckily I don‘t think this program screws up anything more than your terminal.