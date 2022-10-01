#!/bin/sh
pv --quiet --line-mode --rate-limit 20000 < bench/nom-shell-452e7d5.nix-log | cabal run -- nom --json +RTS -s
