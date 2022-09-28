#!/bin/sh

set -eu
FOLDER="bench/profile-run-$(date +'%Y-%m-%d-%H:%M:%S')"
mkdir -p "$FOLDER"
git show --oneline -q > "$FOLDER/git-status"
git status >> "$FOLDER/git-status"
git diff >> "$FOLDER/git-status"
cabal run --enable-profiling --ghc-option "-with-rtsopts=-p -l-au -s$FOLDER/allocations" -- nom  < bench/nom-shell-452e7d5.nix-log
mv nom.eventlog "$FOLDER/eventlog"
mv nom.prof "$FOLDER/prof"
hs-speedscope "$FOLDER/eventlog"
echo See results in "$FOLDER/eventlog.json"
