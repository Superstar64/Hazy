module Hazy.PreludeNonEmpty where

import Hazy.Prelude

data NonEmpty a = a :| [a]

infixr 5 :|

instance (Eq a) => Eq (NonEmpty a) where
  (==) = placeholder

instance (Ord a) => Ord (NonEmpty a) where
  compare = placeholder

instance (Read a) => Read (NonEmpty a) where
  readsPrec = placeholder

instance (Show a) => Show (NonEmpty a) where
  showsPrec = placeholder

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| map f xs

instance Applicative NonEmpty where
  pure x = x :| []
  (<*>) = ap

instance Monad NonEmpty where
  m >>= k = sconcat $ fmap k m
