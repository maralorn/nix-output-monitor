# Revision history for nix-output-monitor

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

## 1.0.1.0 -- 2021-02-21

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
