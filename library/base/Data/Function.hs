{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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

import Prelude (error)

id :: a -> a
id = error "todo"

const :: a -> b -> a
const = error "todo"

infixr 9 .

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = error "todo"

flip :: (a -> b -> c) -> b -> a -> c
flip = error "todo"

infixr 0 $

($) :: (a -> b) -> a -> b
($) = error "todo"

infixl 1 &

(&) :: a -> (a -> b) -> b
(&) = error "todo"

fix :: (a -> a) -> a
fix = error "todo"

infixl 0 `on`

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = error "todo"
