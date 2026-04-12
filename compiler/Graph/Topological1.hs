-- |
-- This module implements a special version of Löb with cycle checking. This is
-- also loosely based on depth first sort topological sorting.
--
-- See this article on vanilla loeb: https://github.com/quchen/articles/blob/master/loeb-moeb.md
-- Also see standard topological sort: https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
module Graph.Topological1
  ( Loeb (..),
    Formula (..),
    loeb,
    loebST,
  )
where

import Control.Applicative (liftA)
import Control.Monad.ST (ST, runST, stToIO)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Prelude hiding (fail)

data Mark s t a
  = Unmarked
      { fail :: forall a. a,
        algebra :: t (ST s a) -> ST s a
      }
  | Temporary
      { fail :: forall a. a
      }
  | Permanent
      { result :: a
      }

initialize :: (Functor t) => t (Formula t s a) -> t (ST s (STRef s (Mark s t a)))
initialize = fmap apply
  where
    apply Formula {cycle, run} = newSTRef $! Unmarked {algebra = run, fail = cycle}

visit :: (Functor t) => t (STRef s (Mark s t a)) -> t (ST s a)
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
        Temporary {fail} -> fail
        Permanent {result} -> pure result

data Formula t s a = Formula
  { cycle :: forall a. a,
    run :: t (ST s a) -> ST s a
  }

newtype Loeb t a = Loeb (forall s. t (Formula t s a))

-- |
-- Applicative representing lazy side effects ala the R language. This
-- following law holds:
-- > a *> b = b
newtype LazyIO a = LazyIO {runLazyIO :: IO a}

instance Functor LazyIO where
  fmap = liftA

instance Applicative LazyIO where
  pure a = LazyIO (pure a)
  LazyIO f <*> LazyIO m =
    LazyIO $ do
      f <- unsafeInterleaveIO f
      m <- unsafeInterleaveIO m
      pure (f m)

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

loebST :: (Traversable t) => t (Formula t s a) -> ST s (t a)
loebST spreadsheet = do
  spreadsheet <- sequence $ initialize spreadsheet
  sequence $ visit spreadsheet

testLazy = head $ loeb $ Loeb $ sample : undefined
  where
    sample =
      Formula
        { cycle = undefined,
          run = const $ pure 'a'
        }
