module Hazy.PreludeMonoid where

import Hazy.Prelude

class Semigroup a where
  (<>) :: a -> a -> a
  a <> b = sconcat (a :| [b])

  sconcat :: NonEmpty a -> a
  sconcat = foldr1 (<>)

  stimes :: (Integral b) => b -> a -> a
  stimes = stimesInteger

  stimesInteger :: Integer -> a -> a
  stimesInteger n _
    | n <= 0 = error "Prelude.stimes: negative number"
  stimesInteger n a = go n a
    where
      go 1 a = a
      go n a = go (n - 1) a

class (Semigroup a) => Monoid a where
  mempty :: a
  mempty = mconcat []

  mappend :: a -> a -> a
  mappend = (<>)

  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty
