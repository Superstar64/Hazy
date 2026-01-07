-- | Lazy side effects
module Data.IO.Lazy where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Control.Monad.Fix (MonadFix (..))
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Monad representing lazy side effects ala the R language.
-- This following law holds:
-- > a >> b = b
newtype LazyIO a = LazyIO {runLazyIO :: IO a}

instance Functor LazyIO where
  fmap = liftA

instance Applicative LazyIO where
  pure a = LazyIO (pure a)
  (<*>) = ap

instance Monad LazyIO where
  return = pure
  LazyIO m >>= f = LazyIO $ do
    m <- unsafeInterleaveIO m
    runLazyIO $ f m

instance MonadFix LazyIO where
  mfix go = LazyIO $ mfix (runLazyIO . go)
