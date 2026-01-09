module Main where

import Build_doctests (flags, module_sources, pkgs)
import Relude
import Test.DocTest (doctest)

main :: IO ()
main = doctest args
 where
  args = flags ++ pkgs ++ module_sources
