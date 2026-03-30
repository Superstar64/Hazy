module Main where

import Hazy.Prelude (trace)

data Strict = Strict {select :: !String}

ignore = case Strict (trace "test" "abc") of
  Strict abc -> "ignored"

repack = let Strict repack = Strict "repack" in repack

selector = select (Strict "select")

main = putStrLn (ignore ++ repack ++ selector)
