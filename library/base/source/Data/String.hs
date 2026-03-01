module Data.String
  ( String,
    IsString (..),
    lines,
    words,
    unlines,
    unwords,
  )
where

import Data.Char (Char)
import Hazy (placeholder)
import Prelude (error)

type String = [Char]

class IsString a where
  fromString :: String -> a

lines :: String -> [String]
lines = placeholder

words :: String -> [String]
words = placeholder

unlines :: [String] -> String
unlines = placeholder

unwords :: [String] -> String
unwords = placeholder
