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
import Hazy.Prelude (String, lines, placeholder, unlines, unwords, words)
import Prelude (error)

class IsString a where
  fromString :: String -> a
