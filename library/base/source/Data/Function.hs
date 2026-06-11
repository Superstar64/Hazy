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

import Hazy.Prelude (placeholder)

infixl 1 &

(&) :: a -> (a -> b) -> b
(&) = placeholder

fix :: (a -> a) -> a
fix = placeholder

infixl 0 `on`

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = placeholder
