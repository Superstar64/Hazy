module Data.Tuple
  ( Solo (..),
    fst,
    snd,
    curry,
    uncurry,
    swap,
  )
where

import Prelude (error)

data Solo a = Solo a

fst :: (a, b) -> a
fst = error "todo"

snd :: (a, b) -> b
snd = error "todo"

curry :: ((a, b) -> c) -> a -> b -> c
curry = error "todo"

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = error "todo"

swap :: (a, b) -> (b, a)
swap = error "todo"
