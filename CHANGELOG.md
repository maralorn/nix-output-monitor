# Revision history for nix-output-monitor

## 2.0.1 -- ???

* More consistent table alignment calculation (thanks to @9999years)
  * Alignment issues on WSL, macOS or kitty should be gone now, please report any issues you encounter
* Correct SIGINT/SIGTERM handling (thanks to @picnoir)
* Use bold font in status line
* Significant performance fixes

## 2.0.0.7 -- 2023-09-17

* Bump hermes-json dependency to 0.6.0.0

## 2.0.0.6 -- 2023-05-14

* Small improvements in error reporting.
* Significant performance improvements.

## 2.0.0.5 -- 2022-11-28

* Fix a bug, where nom failed to parse build errors in json mode.
* Improve test suite, including regressions for this bug.

## 2.0.0.4 -- 2022-11-21

* nom will now not show json parsing errors when something was not a json message from nix. #69
* `nom develop` and `nom shell` will now work when the user specifies a `--command` #69
* nom will now pass through lines without newline ending, when in old style piping mode. This way it can e.g. show sudo prompts. #68

## 2.0.0.3 -- 2022-10-25

* Fix crash on terminal misreporting it’s size
* Fix crash, when nix-command experimental feature is not enabled.
* Performance improvements

## 2.0.0.2 -- 2022-10-19

* Fix crash on too small windows

## 2.0.0.1 -- 2022-10-18

* Fix formatting for transfer host labels.
* Declutter tree: Only show timers over 1s. Reduce host label colors.

## 2.0.0.0 -- 2022-10-15

### Highlights:

* **New ways to use nom**, via different aliases and options. Have a look at the README for new usage or just try `nom build`, `nom develop` or `nom-build` …
* **Full support for new-style nix commands like `nix build`** and therefor also flakes.
* Support for parsing the nix "internal-json" log format. This gives us much more information.
* The output has been massively reworked to accommodate the new information available from json output. This includes:
  * Running downloads/uploads
  * Show current build phase (only possible for local builds).
  * Remote builders are displayed more economically
  * Build summaries have been reworked to be less overwhelming
  * Log output is prefixed with build job names.
* Massive internal refactoring with significant performance improvements and less flickering.

### Further changes:

* The algorithm to layout the rendering tree has been improved.
* Improved build name display and show build platform if different from our platform.
* Better error reporting.
* Pause build time counter while system is suspended.
* Fixed a color flickering issue in the dependency graph (thx @alyssais).
* Removed some weird operators. (thx @blachheaven)
* The old nom-build wrapper is obsolete and has been removed.
* Updated to use ghc 9.2 with corresponding features like GHC2021 and RecordDotSyntax.
* Added benchmarking and profiling scripts, to monitor performance.
* Most performance improvements came from replacing aeson with json-hermes.

## 1.1.3.0 -- 2022-03-21
* Update parser to correctly detect failed builds on nix 2.7

## 1.1.2.1 -- 2022-03-16
* Move nom-build and zsh completion files from nixpkgs into this repo
* Internal refactoring for streamly >= 0.8 and ghc 9.0 compat

## 1.1.2.0 -- 2022-03-12
* Fix the bug that the colored errors of newer nix version didn‘t get parsed as errors.

## 1.1.1.0 -- 2022-03-08
* Only show dependency graph when necessary
* Only show build counts for host, when not zero

## 1.1.0.0 -- 2022-03-07
* Replace list of running and failed builds with a continually updated dependency graph
* A lot of small convenience improvements e.g. nicer timestamps
* Make input parsing more robust via using streamly. This hopefully fixes #23.
* Symbols: Change a few used symbols and force text representation

## 1.0.5.0 -- 2022-03-05
* Make the parser for storepath accept more storepaths which actually occur in the wild.

## 1.0.4.2 -- 2022-02-25
* Other fixes for relude 1.0 compat

## 1.0.4.1 -- 2022-02-25
* Rename an internal variable for relude 1.0 compat

## 1.0.4.0 -- 2021-12-03
* Make parsing a bit more flexible for better nix 2.4 compatibility.

## 1.0.3.3 -- 2021-09-24
* Reduce flickering for some terminal emulators. Thanks @pennae

## 1.0.3.2 -- 2021-09-17
* Improve warning when nom received no input, again.

## 1.0.3.1 -- 2021-04-30
* Improve warning when nom received no input

## 1.0.3.0 -- 2021-03-04

* Internal refactoring
* State of last planned build is now displayed in bottom bar

## 1.0.2.0 -- 2021-03-04

### Bug fixes

* Introduce proper file locking for build times DB. Multiple running nom instances should work now with every single build time being recorded.
* Improved the parser for failed build messages. Should now correctly work with `nix-build -k`.

## 1.0.1.1 -- 2021-02-21

* Use a different symbol for the total

## 1.0.1.0 -- 2021-02-21

* Catch IO errors and try to restart

## 1.0.0.0 -- 2021-02-21

* Added recognition of `--check` builds
* Added recognition of failed builds
* Display final derivation in status line
* Exit with failure code when a failed build was recognized
* Truncate output so that it works in too small terminal windows
* Save past build times in cache and display the moving average to the user

## 0.1.0.3 -- 2021-02-20

* Reworked the printing code to make it more robust

## 0.1.0.2 -- 2020-10-18

* Fixed a layout bug when no builds are going on.

## 0.1.0.1 -- 2020-10-16

* Changed emojis for completed to checkmark and waiting to hourglass.

## 0.1.0.0 -- 2020-10-03

* First version. Released on an unsuspecting world.
