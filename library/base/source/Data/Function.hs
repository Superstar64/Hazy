module Data.Function
  ( id,
    const,
    (.),
    flip,
    ($),
    (&),
    fix,
    on,
  )
where

import Hazy (const, flip, id, placeholder, (.))
import Prelude (error)

infixr 0 $

($) :: (a -> b) -> a -> b
($) = placeholder

infixl 1 &

(&) :: a -> (a -> b) -> b
(&) = placeholder

fix :: (a -> a) -> a
fix = placeholder

infixl 0 `on`

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = placeholder
