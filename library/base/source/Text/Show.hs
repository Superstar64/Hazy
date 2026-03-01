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
import Hazy (placeholder)
import Prelude (error)

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

shows :: (Show a) => a -> ShowS
shows = placeholder

showChar :: Char -> ShowS
showChar = placeholder

showString :: String -> ShowS
showString = placeholder

showParen :: Bool -> ShowS -> ShowS
showParen = placeholder

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith = placeholder
