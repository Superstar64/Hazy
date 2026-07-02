module Data.Functor.Classes where

import Hazy.Prelude (placeholder)

class Show1 f where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
  liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS

showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = placeholder
