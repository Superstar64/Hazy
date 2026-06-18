module DoubleApp where

import Prelude ()

class Go a where
  go :: a -> ()

data T2 f a = MkT2 (f (f a))

-- bad :: Go (f (f a)) => T2 f a -> ()
bad (MkT2 a) = go a
