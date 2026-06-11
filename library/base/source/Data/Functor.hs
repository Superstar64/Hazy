module Data.Functor
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
    void,
  )
where

import Hazy.Prelude (placeholder)

infixl 4 $>

($>) :: (Functor f) => f a -> b -> f b
($>) = placeholder

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = placeholder

void :: (Functor f) => f a -> f ()
void = placeholder
