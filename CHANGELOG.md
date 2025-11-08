# Revision history for nix-output-monitor

## 2.1.8 -- 2025-11-08

* Fix bug in history limit enforcement

## 2.1.7 -- 2025-11-08

## Highlights

* Fixed support for build time estimates. The guess is now the median of the last 10 builds (by drv name without hash and build host name).
* Fixes compatibility with some additions in nix internal-json format.
* #200 hopefully fixes download reporting for newer nix versions. (Haven’t yet tested on that versions, waiting for user feedback.)

## What's Changed
* Add missing result type 108 fetchStatus by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/194
* fix: typographical errors by @sheeeng in https://github.com/maralorn/nix-output-monitor/pull/199
* Use singular "Trace" when count is 1 by @osbm in https://github.com/maralorn/nix-output-monitor/pull/202
* fix: Local store URL parsing (#200) by @blackheaven in https://github.com/maralorn/nix-output-monitor/pull/203
* Fix build on systems other than x86_64-linux by @psentee in https://github.com/maralorn/nix-output-monitor/pull/204
* Switch csv cache location by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/207
* Use repeatedly by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/208
* Use median of last 20 builds for estimate by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/209
* Move toHost/fromHost by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/210
* Avoid doing lazy IO on build-reports.csv by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/211
* Rename field in csv to be clearer by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/213
* Add fourmolu to shell to fix formatting discrepancies by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/214
* More reordering by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/215
* completions: add fish shell support by @considerate in https://github.com/maralorn/nix-output-monitor/pull/188
* Improve download size parsing by @maralorn in https://github.com/maralorn/nix-output-monitor/pull/216

## New Contributors
* @sheeeng made their first contribution in https://github.com/maralorn/nix-output-monitor/pull/199
* @osbm made their first contribution in https://github.com/maralorn/nix-output-monitor/pull/202
* @psentee made their first contribution in https://github.com/maralorn/nix-output-monitor/pull/204
* @considerate made their first contribution in https://github.com/maralorn/nix-output-monitor/pull/188

**Full Changelog**: https://github.com/maralorn/nix-output-monitor/compare/v2.1.6...v2.1.7

## 2.1.6 -- 2025-04-07

* Improve finished message coloring by @Aehmlo
* Switch from lock-file to filelock library
* Compatibility with GHC 9.8

## 2.1.5 -- 2025-02-28

* Support new activity type used in nix
* Use synchronized rendering to better prevent flickering. See <https://gitlab.com/gnachman/iterm2/-/wikis/synchronized-updates-spec> for more details.
  Note: For a list of supported Terminals see <https://gist.github.com/christianparpart/d8a62cc1ab659194337d73e399004036#adoption-state>

## 2.1.4 -- 2024-10-17

* bash: Add shell completion for `nom` command (thanks to @tomberek and @pdietl).
* zsh: Add shell completion for `nom` and `nom-shell` command (additionally to the existing completion for `nom-build`).
* Fix typo in README (thanks to @techie2000)

## 2.1.3 -- 2024-07-28

* Remove non ascii characters from help message (thanks to @SandaruKasa)
* Display trace/warning messages (thanks to @daniel-sampliner)
* Internal: Flake updates (thanks to @SuperSandro2000)

## 2.1.2 -- 2024-01-26

* Compatibility with mtl 2.3

## 2.1.1 -- 2023-11-27

* Maintenance: Only use `-Werror` in CI, not in release

## 2.1.0 -- 2023-11-26

* More consistent table alignment calculation (thanks to @9999years)
  * Alignment issues on WSL, macOS or kitty should be gone now, please report any issues you encounter
  * This changes the icon for waiting builds and downloads to use the pause symbol
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
