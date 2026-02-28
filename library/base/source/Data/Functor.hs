module Data.Functor
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
    void,
  )
where

import Prelude (error)

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  infixl 4 <$
  (<$) :: a -> f b -> f a

infixl 4 $>

($>) :: (Functor f) => f a -> b -> f b
($>) = error "todo"

infixl 4 <$>

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = error "todo"

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = error "todo"

void :: (Functor f) => f a -> f ()
void = error "todo"
