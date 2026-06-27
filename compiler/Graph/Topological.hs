-- |
-- This module implements a special version of Löb with cycle checking. This is
-- also loosely based on depth first sort topological sorting.
--
-- See this article on vanilla loeb: https://github.com/quchen/articles/blob/master/loeb-moeb.md
-- Also see standard topological sort: https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
module Graph.Topological
  ( Formula1 (Formula1),
    Loeb1 (..),
    loeb1,
    loebST1,
    Formula2 (Formula2),
    Loeb2 (..),
    loeb2,
    loebST2,
    Formula3 (Formula3),
    Loeb3 (..),
    loeb3,
    loebST3,
    Formula4 (Formula4),
    Loeb4 (..),
    loeb4,
    loebST4,
    Formula5 (Formula5),
    Loeb5 (..),
    loeb5,
    loebST5,
    Formula6 (Formula6),
    Loeb6 (..),
    loeb6,
    loebST6,
    Formula7 (Formula7),
    Loeb7 (..),
    loeb7,
    loebST7,
    Formula8 (Formula8),
    Loeb8 (..),
    loeb8,
    loebST8,
  )
where

import Control.Applicative (liftA)
import Control.Monad.ST (ST, stToIO)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor2 (Functor2 (..), fmap2)
import Data.Heptafunctor (Heptafunctor (..))
import Data.Heptatraversable (Heptatraversable (..))
import Data.Hexafunctor (Hexafunctor (..))
import Data.Hexatraversable (Hexatraversable (..))
import Data.NaturalTransformation (NaturalTransformation (..))
import Data.Octafunctor (Octafunctor (..))
import Data.Octatraversable (Octatraversable (..))
import Data.Pentafunctor (Pentafunctor (..))
import Data.Pentatraversable (Pentatraversable (..))
import Data.Quadrifunctor (Quadrifunctor (..))
import Data.Quadritraversable (Quadritraversable (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Traversable2 (Traversable2 (..), sequence2', traverse2')
import Data.Trifunctor (Trifunctor (..))
import Data.Tritraversable (Tritraversable (..))
import Graph.Topological1 (Formula1 (Formula1))
import qualified Graph.Topological1
import Graph.Topological2 (Formula2 (Formula2))
import qualified Graph.Topological2
import Graph.Topological3 (Formula3 (Formula3))
import qualified Graph.Topological3
import Graph.Topological4 (Formula4 (Formula4))
import qualified Graph.Topological4
import Graph.Topological5 (Formula5 (Formula5))
import qualified Graph.Topological5
import Graph.Topological6 (Formula6 (Formula6))
import qualified Graph.Topological6
import Graph.Topological7 (Formula7 (Formula7))
import qualified Graph.Topological7
import Graph.Topological8 (Formula8 (Formula8))
import qualified Graph.Topological8
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

newtype Loeb3 t a b c
  = Loeb3
      ( forall s.
        t
          (Formula3 t s a b c a)
          (Formula3 t s a b c b)
          (Formula3 t s a b c c)
      )

newtype Three t a b c m = Three {runThree :: t (m a) (m b) (m c)}

instance (Trifunctor t) => Functor2 (Three t a b c) where
  fmap2 (Morph f) (Three x) = Three $ trimap f f f x

instance (Tritraversable t) => Traversable2 (Three t a b c) where
  traverse2 (Morph f) (Three x) =
    Three
      <$> tritraverse
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        x

formula3 :: Formula3 t s a b c z -> Formula (Three t a b c) s z
formula3 Formula3 {cycle, run} = Formula {cycle, run = run . runThree}

pack3 ::
  (Trifunctor t) =>
  t
    (Formula3 t s a b c z1)
    (Formula3 t s a b c z2)
    (Formula3 t s a b c z3) ->
  t
    (Formula (Three t a b c) s z1)
    (Formula (Three t a b c) s z2)
    (Formula (Three t a b c) s z3)
pack3 = trimap formula3 formula3 formula3

unpack3 ::
  (Trifunctor t) =>
  t
    (Identity a)
    (Identity b)
    (Identity c) ->
  t a b c
unpack3 = trimap runIdentity runIdentity runIdentity

loeb3 ::
  (Tritraversable t) =>
  Loeb3 t a b c ->
  t a b c
loeb3 (Loeb3 spreadsheet) = unpack3 $ runThree $ loeb $ Loeb $ Three $ pack3 spreadsheet

loebST3 ::
  (Tritraversable t) =>
  t
    (Formula3 t s a b c a)
    (Formula3 t s a b c b)
    (Formula3 t s a b c c) ->
  ST s (t a b c)
loebST3 spreadsheet = fmap (unpack3 . runThree) $ loebST $ Three $ pack3 spreadsheet

newtype Loeb4 t a b c d
  = Loeb4
      ( forall s.
        t
          (Formula4 t s a b c d a)
          (Formula4 t s a b c d b)
          (Formula4 t s a b c d c)
          (Formula4 t s a b c d d)
      )

newtype Four t a b c d m = Four {runFour :: t (m a) (m b) (m c) (m d)}

instance (Quadrifunctor t) => Functor2 (Four t a b c d) where
  fmap2 (Morph f) (Four x) = Four $ quadrimap f f f f x

instance (Quadritraversable t) => Traversable2 (Four t a b c d) where
  traverse2 (Morph f) (Four x) =
    Four
      <$> quadritraverse
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        x

formula4 :: Formula4 t s a b c d z -> Formula (Four t a b c d) s z
formula4 Formula4 {cycle, run} = Formula {cycle, run = run . runFour}

pack4 ::
  (Quadrifunctor t) =>
  t
    (Formula4 t s a b c d z1)
    (Formula4 t s a b c d z2)
    (Formula4 t s a b c d z3)
    (Formula4 t s a b c d z4) ->
  t
    (Formula (Four t a b c d) s z1)
    (Formula (Four t a b c d) s z2)
    (Formula (Four t a b c d) s z3)
    (Formula (Four t a b c d) s z4)
pack4 = quadrimap formula4 formula4 formula4 formula4

unpack4 ::
  (Quadrifunctor t) =>
  t
    (Identity a)
    (Identity b)
    (Identity c)
    (Identity d) ->
  t a b c d
unpack4 = quadrimap runIdentity runIdentity runIdentity runIdentity

loeb4 ::
  (Quadritraversable t) =>
  Loeb4 t a b c d ->
  t a b c d
loeb4 (Loeb4 spreadsheet) = unpack4 $ runFour $ loeb $ Loeb $ Four $ pack4 spreadsheet

loebST4 ::
  (Quadritraversable t) =>
  t
    (Formula4 t s a b c d a)
    (Formula4 t s a b c d b)
    (Formula4 t s a b c d c)
    (Formula4 t s a b c d d) ->
  ST s (t a b c d)
loebST4 spreadsheet = fmap (unpack4 . runFour) $ loebST $ Four $ pack4 spreadsheet

newtype Loeb5 t a b c d e
  = Loeb5
      ( forall s.
        t
          (Formula5 t s a b c d e a)
          (Formula5 t s a b c d e b)
          (Formula5 t s a b c d e c)
          (Formula5 t s a b c d e d)
          (Formula5 t s a b c d e e)
      )

newtype Five t a b c d e m = Five {runFive :: t (m a) (m b) (m c) (m d) (m e)}

instance (Pentafunctor t) => Functor2 (Five t a b c d e) where
  fmap2 (Morph f) (Five x) = Five $ pentamap f f f f f x

instance (Pentatraversable t) => Traversable2 (Five t a b c d e) where
  traverse2 (Morph f) (Five x) =
    Five
      <$> pentatraverse
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        x

formula5 :: Formula5 t s a b c d e z -> Formula (Five t a b c d e) s z
formula5 Formula5 {cycle, run} = Formula {cycle, run = run . runFive}

pack5 ::
  (Pentafunctor t) =>
  t
    (Formula5 t s a b c d e z1)
    (Formula5 t s a b c d e z2)
    (Formula5 t s a b c d e z3)
    (Formula5 t s a b c d e z4)
    (Formula5 t s a b c d e z5) ->
  t
    (Formula (Five t a b c d e) s z1)
    (Formula (Five t a b c d e) s z2)
    (Formula (Five t a b c d e) s z3)
    (Formula (Five t a b c d e) s z4)
    (Formula (Five t a b c d e) s z5)
pack5 = pentamap formula5 formula5 formula5 formula5 formula5

unpack5 ::
  (Pentafunctor t) =>
  t
    (Identity a)
    (Identity b)
    (Identity c)
    (Identity d)
    (Identity e) ->
  t a b c d e
unpack5 = pentamap runIdentity runIdentity runIdentity runIdentity runIdentity

loeb5 ::
  (Pentatraversable t) =>
  Loeb5 t a b c d e ->
  t a b c d e
loeb5 (Loeb5 spreadsheet) = unpack5 $ runFive $ loeb $ Loeb $ Five $ pack5 spreadsheet

loebST5 ::
  (Pentatraversable t) =>
  t
    (Formula5 t s a b c d e a)
    (Formula5 t s a b c d e b)
    (Formula5 t s a b c d e c)
    (Formula5 t s a b c d e d)
    (Formula5 t s a b c d e e) ->
  ST s (t a b c d e)
loebST5 spreadsheet = fmap (unpack5 . runFive) $ loebST $ Five $ pack5 spreadsheet

newtype Loeb6 t a b c d e f
  = Loeb6
      ( forall s.
        t
          (Formula6 t s a b c d e f a)
          (Formula6 t s a b c d e f b)
          (Formula6 t s a b c d e f c)
          (Formula6 t s a b c d e f d)
          (Formula6 t s a b c d e f e)
          (Formula6 t s a b c d e f f)
      )

newtype Six t a b c d e f m = Six {runSix :: t (m a) (m b) (m c) (m d) (m e) (m f)}

instance (Hexafunctor t) => Functor2 (Six t a b c d e f) where
  fmap2 (Morph f) (Six x) = Six $ hexamap f f f f f f x

instance (Hexatraversable t) => Traversable2 (Six t a b c d e f) where
  traverse2 (Morph f) (Six x) =
    Six
      <$> hexatraverse
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        x

formula6 :: Formula6 t s a b c d e f z -> Formula (Six t a b c d e f) s z
formula6 Formula6 {cycle, run} = Formula {cycle, run = run . runSix}

pack6 ::
  (Hexafunctor t) =>
  t
    (Formula6 t s a b c d e f z1)
    (Formula6 t s a b c d e f z2)
    (Formula6 t s a b c d e f z3)
    (Formula6 t s a b c d e f z4)
    (Formula6 t s a b c d e f z5)
    (Formula6 t s a b c d e f z6) ->
  t
    (Formula (Six t a b c d e f) s z1)
    (Formula (Six t a b c d e f) s z2)
    (Formula (Six t a b c d e f) s z3)
    (Formula (Six t a b c d e f) s z4)
    (Formula (Six t a b c d e f) s z5)
    (Formula (Six t a b c d e f) s z6)
pack6 = hexamap formula6 formula6 formula6 formula6 formula6 formula6

unpack6 ::
  (Hexafunctor t) =>
  t
    (Identity a)
    (Identity b)
    (Identity c)
    (Identity d)
    (Identity e)
    (Identity f) ->
  t a b c d e f
unpack6 = hexamap runIdentity runIdentity runIdentity runIdentity runIdentity runIdentity

loeb6 ::
  (Hexatraversable t) =>
  Loeb6 t a b c d e f ->
  t a b c d e f
loeb6 (Loeb6 spreadsheet) = unpack6 $ runSix $ loeb $ Loeb $ Six $ pack6 spreadsheet

loebST6 ::
  (Hexatraversable t) =>
  t
    (Formula6 t s a b c d e f a)
    (Formula6 t s a b c d e f b)
    (Formula6 t s a b c d e f c)
    (Formula6 t s a b c d e f d)
    (Formula6 t s a b c d e f e)
    (Formula6 t s a b c d e f f) ->
  ST s (t a b c d e f)
loebST6 spreadsheet = fmap (unpack6 . runSix) $ loebST $ Six $ pack6 spreadsheet

newtype Loeb7 t a b c d e f g
  = Loeb7
      ( forall s.
        t
          (Formula7 t s a b c d e f g a)
          (Formula7 t s a b c d e f g b)
          (Formula7 t s a b c d e f g c)
          (Formula7 t s a b c d e f g d)
          (Formula7 t s a b c d e f g e)
          (Formula7 t s a b c d e f g f)
          (Formula7 t s a b c d e f g g)
      )

newtype Seven t a b c d e f g m = Seven {runSeven :: t (m a) (m b) (m c) (m d) (m e) (m f) (m g)}

instance (Heptafunctor t) => Functor2 (Seven t a b c d e f g) where
  fmap2 (Morph f) (Seven x) = Seven $ heptamap f f f f f f f x

instance (Heptatraversable t) => Traversable2 (Seven t a b c d e f g) where
  traverse2 (Morph f) (Seven x) =
    Seven
      <$> heptatraverse
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        x

formula7 :: Formula7 t s a b c d e f g z -> Formula (Seven t a b c d e f g) s z
formula7 Formula7 {cycle, run} = Formula {cycle, run = run . runSeven}

pack7 ::
  (Heptafunctor t) =>
  t
    (Formula7 t s a b c d e f g z1)
    (Formula7 t s a b c d e f g z2)
    (Formula7 t s a b c d e f g z3)
    (Formula7 t s a b c d e f g z4)
    (Formula7 t s a b c d e f g z5)
    (Formula7 t s a b c d e f g z6)
    (Formula7 t s a b c d e f g z7) ->
  t
    (Formula (Seven t a b c d e f g) s z1)
    (Formula (Seven t a b c d e f g) s z2)
    (Formula (Seven t a b c d e f g) s z3)
    (Formula (Seven t a b c d e f g) s z4)
    (Formula (Seven t a b c d e f g) s z5)
    (Formula (Seven t a b c d e f g) s z6)
    (Formula (Seven t a b c d e f g) s z7)
pack7 = heptamap formula7 formula7 formula7 formula7 formula7 formula7 formula7

unpack7 ::
  (Heptafunctor t) =>
  t
    (Identity a)
    (Identity b)
    (Identity c)
    (Identity d)
    (Identity e)
    (Identity f)
    (Identity g) ->
  t a b c d e f g
unpack7 = heptamap runIdentity runIdentity runIdentity runIdentity runIdentity runIdentity runIdentity

loeb7 ::
  (Heptatraversable t) =>
  Loeb7 t a b c d e f g ->
  t a b c d e f g
loeb7 (Loeb7 spreadsheet) = unpack7 $ runSeven $ loeb $ Loeb $ Seven $ pack7 spreadsheet

loebST7 ::
  (Heptatraversable t) =>
  t
    (Formula7 t s a b c d e f g a)
    (Formula7 t s a b c d e f g b)
    (Formula7 t s a b c d e f g c)
    (Formula7 t s a b c d e f g d)
    (Formula7 t s a b c d e f g e)
    (Formula7 t s a b c d e f g f)
    (Formula7 t s a b c d e f g g) ->
  ST s (t a b c d e f g)
loebST7 spreadsheet = fmap (unpack7 . runSeven) $ loebST $ Seven $ pack7 spreadsheet

newtype Loeb8 t a b c d e f g h
  = Loeb8
      ( forall s.
        t
          (Formula8 t s a b c d e f g h a)
          (Formula8 t s a b c d e f g h b)
          (Formula8 t s a b c d e f g h c)
          (Formula8 t s a b c d e f g h d)
          (Formula8 t s a b c d e f g h e)
          (Formula8 t s a b c d e f g h f)
          (Formula8 t s a b c d e f g h g)
          (Formula8 t s a b c d e f g h h)
      )

newtype Eight t a b c d e f g h m = Eight {runEight :: t (m a) (m b) (m c) (m d) (m e) (m f) (m g) (m h)}

instance (Octafunctor t) => Functor2 (Eight t a b c d e f g h) where
  fmap2 (Morph f) (Eight x) = Eight $ octamap f f f f f f f f x

instance (Octatraversable t) => Traversable2 (Eight t a b c d e f g h) where
  traverse2 (Morph f) (Eight x) =
    Eight
      <$> octatraverse
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        (getCompose . f)
        x

formula8 :: Formula8 t s a b c d e f g h z -> Formula (Eight t a b c d e f g h) s z
formula8 Formula8 {cycle, run} = Formula {cycle, run = run . runEight}

pack8 ::
  (Octafunctor t) =>
  t
    (Formula8 t s a b c d e f g h z1)
    (Formula8 t s a b c d e f g h z2)
    (Formula8 t s a b c d e f g h z3)
    (Formula8 t s a b c d e f g h z4)
    (Formula8 t s a b c d e f g h z5)
    (Formula8 t s a b c d e f g h z6)
    (Formula8 t s a b c d e f g h z7)
    (Formula8 t s a b c d e f g h z8) ->
  t
    (Formula (Eight t a b c d e f g h) s z1)
    (Formula (Eight t a b c d e f g h) s z2)
    (Formula (Eight t a b c d e f g h) s z3)
    (Formula (Eight t a b c d e f g h) s z4)
    (Formula (Eight t a b c d e f g h) s z5)
    (Formula (Eight t a b c d e f g h) s z6)
    (Formula (Eight t a b c d e f g h) s z7)
    (Formula (Eight t a b c d e f g h) s z8)
pack8 = octamap formula8 formula8 formula8 formula8 formula8 formula8 formula8 formula8

unpack8 ::
  (Octafunctor t) =>
  t
    (Identity a)
    (Identity b)
    (Identity c)
    (Identity d)
    (Identity e)
    (Identity f)
    (Identity g)
    (Identity h) ->
  t a b c d e f g h
unpack8 = octamap runIdentity runIdentity runIdentity runIdentity runIdentity runIdentity runIdentity runIdentity

loeb8 ::
  (Octatraversable t) =>
  Loeb8 t a b c d e f g h ->
  t a b c d e f g h
loeb8 (Loeb8 spreadsheet) = unpack8 $ runEight $ loeb $ Loeb $ Eight $ pack8 spreadsheet

loebST8 ::
  (Octatraversable t) =>
  t
    (Formula8 t s a b c d e f g h a)
    (Formula8 t s a b c d e f g h b)
    (Formula8 t s a b c d e f g h c)
    (Formula8 t s a b c d e f g h d)
    (Formula8 t s a b c d e f g h e)
    (Formula8 t s a b c d e f g h f)
    (Formula8 t s a b c d e f g h g)
    (Formula8 t s a b c d e f g h h) ->
  ST s (t a b c d e f g h)
loebST8 spreadsheet = fmap (unpack8 . runEight) $ loebST $ Eight $ pack8 spreadsheet
