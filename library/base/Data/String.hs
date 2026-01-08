{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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
import Prelude (error)

type String = [Char]

class IsString a where
  fromString :: String -> a

lines :: String -> [String]
lines = error "todo"

words :: String -> [String]
words = error "todo"

unlines :: [String] -> String
unlines = error "todo"

unwords :: [String] -> String
unwords = error "todo"
