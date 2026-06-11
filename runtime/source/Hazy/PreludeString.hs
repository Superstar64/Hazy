module Hazy.PreludeString where

import Hazy.Prelude

data Text

instance Eq Text where
  (==) = placeholder

instance Ord Text where
  compare = placeholder

instance Semigroup Text where
  (<>) = placeholder

instance Monoid Text where
  mempty = placeholder

instance Read Text where
  readsPrec = placeholder

instance Show Text where
  showsPrec = placeholder
