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

import Hazy.Prelude (placeholder)

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith = placeholder
