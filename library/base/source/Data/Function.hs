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

import Hazy (placeholder)
import Prelude (error)

id :: a -> a
id = placeholder

const :: a -> b -> a
const = placeholder

infixr 9 .

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = placeholder

flip :: (a -> b -> c) -> b -> a -> c
flip = placeholder

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
