module Data.Acyclic3
  ( Loeb3 (..),
    loeb3,
    loebST3,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Trifoldable (Trifoldable (..))
import Data.Trifunctor (Trifunctor (..))
import Data.Tritraversable (Tritraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Triple f a = Triple {runTriple :: f a a a}

instance (Trifunctor f) => Functor (Triple f) where
  fmap f (Triple triple) = Triple (trimap f f f triple)

instance (Trifoldable f) => Foldable (Triple f) where
  foldMap f (Triple triple) = trifoldMap f f f triple

instance (Tritraversable f) => Traversable (Triple f) where
  traverse f (Triple triple) = Triple <$> tritraverse f f f triple

data Three a b c
  = ThreeA !a
  | ThreeB !b
  | ThreeC !c

threeA (ThreeA a) = a
threeA _ = undefined

threeB (ThreeB b) = b
threeB _ = undefined

threeC (ThreeC c) = c
threeC _ = undefined

packTripleM :: (Trifunctor f, Functor m) => f (m a) (m b) (m c) -> Triple f (m (Three a b c))
packTripleM = Triple . trimap (fmap ThreeA) (fmap ThreeB) (fmap ThreeC)

unpackTriple :: (Trifunctor t) => Triple t (Three a b c) -> t a b c
unpackTriple = trimap threeA threeB threeC . runTriple

unpackTripleM :: (Trifunctor f, Functor m) => Triple f (m (Three a b c)) -> f (m a) (m b) (m c)
unpackTripleM = trimap (fmap threeA) (fmap threeB) (fmap threeC) . runTriple

goTriple cell = case cell of
  ThreeA run -> fmap ThreeA . run . unpackTripleM
  ThreeB run -> fmap ThreeB . run . unpackTripleM
  ThreeC run -> fmap ThreeC . run . unpackTripleM

newtype Loeb3 f s a b c
  = Loeb3
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) (ST s c) -> ST s a)
          (Void, f (ST s a) (ST s b) (ST s c) -> ST s b)
          (Void, f (ST s a) (ST s b) (ST s c) -> ST s c)
      )

loeb3 ::
  (Tritraversable f) =>
  Loeb3 f s a b c ->
  f a b c
loeb3 (Loeb3 spreadsheet) = unpackTriple $ loeb $ Loeb $ fmap goTriple <$> packTripleM spreadsheet

loebST3 ::
  (Tritraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) -> ST s c) ->
  ST s (t a b c)
loebST3 spreadsheet = fmap unpackTriple $ loebST $ fmap goTriple <$> packTripleM spreadsheet
