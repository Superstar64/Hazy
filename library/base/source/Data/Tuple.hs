module Data.Tuple
  ( Solo (..),
    fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

import Hazy (placeholder)
import Prelude (error)

data Solo a = Solo a

fst :: (a, b) -> a
fst = placeholder

snd :: (a, b) -> b
snd = placeholder

curry :: ((a, b) -> c) -> a -> b -> c
curry = placeholder

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = placeholder

swap :: (a, b) -> (b, a)
swap = placeholder
