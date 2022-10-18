#!/bin/sh

set -eu
FOLDER="bench/profile-run-$(date +'%Y-%m-%d-%H:%M:%S')"
mkdir -p "$FOLDER"
git show --oneline -q > "$FOLDER/git-status"
git status >> "$FOLDER/git-status"
git diff >> "$FOLDER/git-status"
zstd -d < bench/nom-shell-452e7d5.nix-log.zst | cabal run --enable-profiling --profiling-detail=none --ghc-option "-fprof-late" --ghc-option "-with-rtsopts=-pj -s$FOLDER/allocations" -- nom --json
#mv nom.eventlog "$FOLDER/eventlog"
mv nom.prof "$FOLDER/prof.json"
#hs-speedscope "$FOLDER/eventlog"
echo See results in "$FOLDER/prof.json"
