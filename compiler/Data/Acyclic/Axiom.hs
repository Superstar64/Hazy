-- |
-- This module implements a special version of Löb with cycle checking. This is
-- also loosely based on depth first sort topological sorting.
--
-- See this article on vanilla loeb: https://github.com/quchen/articles/blob/master/loeb-moeb.md
-- Also see standard topological sort: https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
module Data.Acyclic.Axiom
  ( loeb,
    Loeb (..),
    loebST,
  )
where

import Control.Monad.ST (ST, runST, stToIO)
import Data.IO.Lazy (LazyIO (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Void (Void, absurd)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (fail)

data Mark s f a
  = Unmarked
      { algebra :: f (ST s a) -> ST s a,
        fail :: Void
      }
  | Temporary
      { fail :: Void
      }
  | Permanent
      { result :: a
      }

initialize :: (Functor f) => f (Void, f (ST s a) -> ST s a) -> f (ST s (STRef s (Mark s f a)))
initialize = fmap apply
  where
    apply (fail, algebra) = newSTRef $! Unmarked {algebra, fail}

visit :: (Functor f) => f (STRef s (Mark s f a)) -> f (ST s a)
visit spreadsheet = table
  where
    table = fmap calculate spreadsheet
    calculate reference = do
      mark <- readSTRef reference
      case mark of
        Unmarked {algebra, fail} -> do
          writeSTRef reference $! Temporary {fail}
          result <- algebra table
          writeSTRef reference $! Permanent {result}
          pure result
        Temporary {fail} -> absurd fail
        Permanent {result} -> pure result

newtype Loeb t a
  = Loeb
      (forall s. t (Void, t (ST s a) -> ST s a))

-- |
-- Cycle tracking version of Löb
--
-- This takes a traversable `t` container a tuple of two elements:
--
-- The exception to throw when a cycle occurs.
--
-- The f-algebra that takes the container itself as an argument.

{-
Laws of traversable ensure that every element is always visited exactly once.
-}
loeb, loebStrict :: (Traversable t) => Loeb t a -> t a
loeb (Loeb spreadsheet) = unsafePerformIO $ do
  spreadsheet <- runLazyIO $ traverse (LazyIO . stToIO) $ initialize spreadsheet
  runLazyIO $ traverse (LazyIO . stToIO) $ visit spreadsheet
loebStrict (Loeb spreadsheet) = runST (loebST spreadsheet)

loebST :: (Traversable t) => t (Void, t (ST s a) -> ST s a) -> ST s (t a)
loebST spreadsheet = do
  spreadsheet <- sequence $ initialize spreadsheet
  sequence $ visit spreadsheet

testLazy = head $ loeb $ Loeb $ (undefined, const $ pure 'a') : undefined
