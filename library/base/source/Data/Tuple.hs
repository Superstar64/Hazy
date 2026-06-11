module Data.Tuple
  ( Solo (..),
    fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

import Hazy.Prelude (placeholder)

data Solo a = Solo a

swap :: (a, b) -> (b, a)
swap = placeholder
