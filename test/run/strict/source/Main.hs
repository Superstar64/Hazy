module Main where

import Hazy.Prelude (pack, trace)

data Strict = Strict {select :: !String}

ignore = case Strict (trace (pack "test") "abc") of
  Strict abc -> "ignored"

repack = let Strict repack = Strict "repack" in repack

selector = select (Strict "select")

combine :: String -> String -> String
combine [] ys = ys
combine (x : xs) ys = x : combine xs ys

main = putStrLn (ignore `combine` repack `combine` selector)
