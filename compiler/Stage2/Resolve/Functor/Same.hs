module Stage2.Resolve.Functor.Same where

import Data.Void (Void, absurd)

class Same a where
  same :: Void -> a -> a -> a

sameDefault :: (Eq a) => Void -> a -> a -> a
sameDefault abort a b
  | a == b = a
  | otherwise = absurd abort

newtype SameA m a = SameA {runSameA :: m a}

instance (Applicative m, Same a) => Same (SameA m a) where
  same abort (SameA left) (SameA right) = SameA (same abort <$> left <*> right)

instance (Same a, Same b) => Same (a, b) where
  same abort (left1, right1) (left2, right2) =
    (same abort left1 left2, same abort right1 right2)

instance Same Void where
  same abort _ _ = absurd abort
