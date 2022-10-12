#!/bin/sh
zstd -d < bench/nom-shell-452e7d5.nix-log.zst | cabal run -- nom --json +RTS -s
