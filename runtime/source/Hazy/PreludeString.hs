module Hazy.PreludeString where

import Hazy.Prelude

data Text

instance Eq Text where
  a == b = unpack a == unpack b

instance Ord Text where
  compare a b = compare (unpack a) (unpack b)

instance Semigroup Text where
  a <> b = pack $ unpack a ++ unpack b

instance Monoid Text where
  mempty = pack ""

instance Read Text where
  readsPrec d string = do
    (literal, string) <- readsPrec d string
    [(pack literal, string)]

instance Show Text where
  showsPrec d = showsPrec d . unpack
