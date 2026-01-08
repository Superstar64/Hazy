{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Text.Show
  ( ShowS,
    Show (..),
    shows,
    showChar,
    showString,
    showParen,
    showListWith,
  )
where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Int (Int)
import Data.String (String)
import Prelude (error)

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

shows :: (Show a) => a -> ShowS
shows = error "todo"

showChar :: Char -> ShowS
showChar = error "todo"

showString :: String -> ShowS
showString = error "todo"

showParen :: Bool -> ShowS -> ShowS
showParen = error "todo"

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith = error "todo"
