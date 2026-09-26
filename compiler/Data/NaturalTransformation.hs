module Data.NaturalTransformation where

newtype NaturalTransformation f g = Morph (forall a. f a -> g a)
