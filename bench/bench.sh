#!/bin/sh
cat bench/nom-shell-452e7d5.nix-log | cabal run nom +RTS -s
