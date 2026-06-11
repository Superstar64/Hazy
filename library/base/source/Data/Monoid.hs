module Data.Monoid
  ( Monoid (..),
    (<>),
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    First (..),
    Last (..),
    Alt (..),
    Ap (..),
  )
where

import Data.Semigroup
  ( All (getAll),
    Any (getAny),
    Dual (getDual),
    Endo (appEndo),
    Product (getProduct),
    Semigroup ((<>)),
    Sum (getSum),
  )
import Hazy.Prelude (Monoid (..), placeholder)

newtype First a = First
  { getFirst :: Maybe a
  }

newtype Last a = Last
  { getLast :: Maybe a
  }

newtype Alt f a = Alt
  { getAlt :: f a
  }

newtype Ap f a = Ap
  { getAp :: f a
  }
