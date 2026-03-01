module Data.Functor
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
    void,
  )
where

import Hazy (placeholder)
import Prelude (error)

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  infixl 4 <$
  (<$) :: a -> f b -> f a

infixl 4 $>

($>) :: (Functor f) => f a -> b -> f b
($>) = placeholder

infixl 4 <$>

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = placeholder

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = placeholder

void :: (Functor f) => f a -> f ()
void = placeholder
