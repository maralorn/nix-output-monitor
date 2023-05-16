#!/bin/sh
zstd -d < bench/nom-shell-452e7d5.nix-log.zst | cabal run --ghc-options="-rtsopts -finfo-table-map -fdistinct-constructor-tables" -- nom --json +RTS -i0.01 -s -l -hi
