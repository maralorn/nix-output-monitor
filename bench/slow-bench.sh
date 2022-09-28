#!/bin/sh
cat bench/nom-shell-452e7d5.nix-log | pv --quiet --line-mode --rate-limit 20000 | cabal run nom +RTS -s
