module Data.Functor2 where

import Data.NaturalTransformation (NaturalTransformation (..))

class Functor2 f where
  fmap2 :: NaturalTransformation a b -> f a -> f b
