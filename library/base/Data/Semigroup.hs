{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Semigroup
  ( Semigroup (..),
    stimesMonoid,
    stimesIdempotent,
    stimesIdempotentMonoid,
    mtimesDefault,
    Min (..),
    Max (..),
    First (..),
    Last (..),
    WrappedMonoid (..),
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    diff,
    cycle1,
    Arg (..),
    ArgMin,
    ArgMax,
  )
where

import Data.Bool (Bool)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Monoid)
import Prelude (Integral, error)

class Semigroup a where
  infixr 6 <>
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: (Integral b) => b -> a -> a

stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid = error "todo"

stimesIdempotent :: (Integral b) => b -> a -> a
stimesIdempotent = error "todo"

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid = error "todo"

mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault = error "todo"

newtype Min a = Min
  { getMin :: a
  }

newtype Max a = Max
  { getMax :: a
  }

newtype First a = First
  { getFirst :: a
  }

newtype Last a = Last
  { getLast :: a
  }

newtype WrappedMonoid m = WrapMonoid
  { unwrapMonoid :: m
  }

newtype Dual a = Dual
  { getDual :: a
  }

newtype Endo a = Endo
  { appEndo :: a -> a
  }

newtype All = All
  { getAll :: Bool
  }

newtype Any = Any
  { getAny :: Bool
  }

newtype Sum a = Sum
  { getSum :: a
  }

newtype Product a = Product
  { getProduct :: a
  }

diff :: (Semigroup m) => m -> Endo m
diff = error "todo"

cycle1 :: (Semigroup m) => m -> m
cycle1 = error "todo"

data Arg a b = Arg a b

type ArgMin a b = Min (Arg a b)

type ArgMax a b = Max (Arg a b)
