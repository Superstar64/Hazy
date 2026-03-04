module Reader where

import Control.Monad (ap, liftM)

data Reader a b = Reader (a -> b)

instance Functor (Reader a) where
  fmap = liftM

instance Applicative (Reader a) where
  pure a = Reader (const a)
  (<*>) = ap

instance Monad (Reader a) where
  Reader f >>= m = Reader $ \s -> let Reader h = m (f s) in h s
