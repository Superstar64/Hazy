module Main where

import Data.Char (GeneralCategory (..), generalCategory)

lowercaseLetter = case generalCategory 'a' of
  LowercaseLetter -> "y"
  _ -> "n"

mathSymbol = case generalCategory '+' of
  MathSymbol -> "y"
  _ -> "n"

main = do
  putStrLn lowercaseLetter
  putStrLn mathSymbol
