-- |
-- This module implements a special version of Löb with cycle checking. This is
-- also loosely based on depth first sort topological sorting.
--
-- See this article on vanilla loeb: https://github.com/quchen/articles/blob/master/loeb-moeb.md
-- Also see standard topological sort: https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
module Graph.Topological
  ( Formula (..),
    Loeb (..),
    loeb,
    loebST,
    Formula1 (Formula1),
    Loeb1 (..),
    loeb1,
    loebST1,
    Formula2 (Formula2),
    Loeb2 (..),
    loeb2,
    loebST2,
  )
where

import Control.Applicative (liftA)
import Control.Monad.ST (ST, stToIO)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor2 (Functor2 (..), fmap2)
import Data.NaturalTransformation (NaturalTransformation (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Traversable2 (Traversable2 (..), sequence2', traverse2')
import Graph.Topological1 (Formula1 (Formula1))
import qualified Graph.Topological1
import Graph.Topological2 (Formula2 (Formula2))
import qualified Graph.Topological2
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

data Mark f s a
  = Unmarked
      { fail :: forall a. a,
        algebra :: f (ST s) -> ST s a
      }
  | Temporary
      { fail :: forall a. a
      }
  | Permanent
      { result :: a
      }

newtype MarkRef f s a = MarkRef (STRef s (Mark f s a))

data Formula f s a = Formula
  { cycle :: forall a. a,
    run :: f (ST s) -> ST s a
  }

initialize :: (Functor2 f) => f (Formula f s) -> f (Compose (ST s) (MarkRef f s))
initialize = fmap2 $ Morph $ \case
  Formula {cycle, run} -> Compose $ do
    reference <- newSTRef $! Unmarked {algebra = run, fail = cycle}
    pure (MarkRef reference)

visit :: forall f s. (Functor2 f) => f (MarkRef f s) -> f (ST s)
visit spreadsheet = table
  where
    table = fmap2 (Morph calculate) spreadsheet
    calculate :: MarkRef f s a -> ST s a
    calculate (MarkRef reference) = do
      mark <- readSTRef reference
      case mark of
        Unmarked {fail, algebra} -> do
          writeSTRef reference $! Temporary {fail}
          result <- algebra table
          writeSTRef reference $! Permanent {result}
          pure result
        Temporary {fail} -> fail
        Permanent {result} -> pure result

loebST :: (Traversable2 t) => t (Formula t s) -> ST s (t Identity)
loebST spreadsheet = do
  spreadsheet <- sequence2 $ initialize spreadsheet
  sequence2' $ visit spreadsheet

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

newtype Loeb f a = Loeb (forall s. f (Formula f s))

mapCompose f (Compose x) = Compose (f x)

loeb :: (Traversable2 f) => Loeb f a -> f Identity
loeb (Loeb spreadsheet) = unsafePerformIO $ do
  spreadsheet <- runLazyIO $ traverse2 (Morph $ mapCompose $ LazyIO . stToIO) $ initialize spreadsheet
  runLazyIO $ traverse2' (Morph $ LazyIO . stToIO) $ visit spreadsheet

newtype Loeb1 t a = Loeb1 (forall s. t (Formula1 t s a))

newtype One t a m = One {runOne :: t (m a)}

instance (Functor t) => Functor2 (One t a) where
  fmap2 (Morph f) (One x) = One $ fmap f x

instance (Traversable t) => Traversable2 (One t a) where
  traverse2 (Morph f) (One x) = One <$> traverse (getCompose . f) x

formula1 :: Formula1 t s a -> Formula (One t a) s a
formula1 Formula1 {cycle, run} = Formula {cycle = cycle, run = run . runOne}

pack1 :: (Functor t) => t (Formula1 t s a) -> t (Formula (One t a) s a)
pack1 = fmap formula1

unpack1 :: (Functor t) => t (Identity a) -> t a
unpack1 = fmap runIdentity

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
loeb1 :: (Traversable t) => Loeb1 t a -> t a
loeb1 (Loeb1 spreadsheet) = unpack1 $ runOne $ loeb $ Loeb $ One (pack1 spreadsheet)

loebST1 :: (Traversable t) => t (Formula1 t s a) -> ST s (t a)
loebST1 spreadsheet = fmap (unpack1 . runOne) $ loebST $ One (pack1 spreadsheet)

newtype Loeb2 t a b
  = Loeb2
      ( forall s.
        t
          (Formula2 t s a b a)
          (Formula2 t s a b b)
      )

newtype Two t a b m = Two {runTwo :: t (m a) (m b)}

instance (Bifunctor t) => Functor2 (Two t a b) where
  fmap2 (Morph f) (Two x) = Two $ bimap f f x

instance (Bitraversable t) => Traversable2 (Two t a b) where
  traverse2 (Morph f) (Two x) = Two <$> bitraverse (getCompose . f) (getCompose . f) x

formula2 :: Formula2 t s a b z -> Formula (Two t a b) s z
formula2 Formula2 {cycle, run} = Formula {cycle, run = run . runTwo}

pack2 ::
  (Bifunctor t) =>
  t
    (Formula2 t s a b z1)
    (Formula2 t s a b z2) ->
  t
    (Formula (Two t a b) s z1)
    (Formula (Two t a b) s z2)
pack2 = bimap formula2 formula2

unpack2 :: (Bifunctor t) => t (Identity a) (Identity b) -> t a b
unpack2 = bimap runIdentity runIdentity

loeb2 ::
  (Bitraversable t) =>
  Loeb2 t a b ->
  t a b
loeb2 (Loeb2 spreadsheet) = unpack2 $ runTwo $ loeb $ Loeb $ Two $ pack2 spreadsheet

loebST2 ::
  (Bitraversable t) =>
  t
    (Formula2 t s a b a)
    (Formula2 t s a b b) ->
  ST s (t a b)
loebST2 spreadsheet = fmap (unpack2 . runTwo) $ loebST $ Two $ pack2 spreadsheet
