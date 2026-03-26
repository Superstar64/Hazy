module Main where

import Hazy.Prelude (pack, trace)

data Strict = Strict {select :: !String}

ignore = case Strict (trace (pack "test") "abc") of
  Strict abc -> "ignored"

repack = let Strict repack = Strict "repack" in repack

selector = select (Strict "select")

main = putStrLn (ignore ++ repack ++ selector)
