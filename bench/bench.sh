#!/bin/sh
zstd -d < bench/nom-shell-452e7d5.nix-log.zst | cabal run --ghc-option=-rtsopts --ghc-option=-eventlog -- nom --json +RTS -s -l -hT
