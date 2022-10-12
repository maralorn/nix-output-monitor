#!/bin/sh
zstd -d < bench/nom-shell-452e7d5.nix-log.zst | pv --quiet --line-mode --rate-limit 20000 | cabal run -- nom --json +RTS -s
