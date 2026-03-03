module Data.Acyclic4
  ( Loeb4 (..),
    loeb4,
    loebST4,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Quadrifoldable (Quadrifoldable (..))
import Data.Quadrifunctor (Quadrifunctor (..))
import Data.Quadritraversable (Quadritraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Quadruple f a = Quadruple {runQuadruple :: f a a a a}

instance (Quadrifunctor f) => Functor (Quadruple f) where
  fmap f (Quadruple quadruple) = Quadruple (quadrimap f f f f quadruple)

instance (Quadrifoldable f) => Foldable (Quadruple f) where
  foldMap f (Quadruple quadruple) = quadrifoldMap f f f f quadruple

instance (Quadritraversable f) => Traversable (Quadruple f) where
  traverse f (Quadruple quadruple) = Quadruple <$> quadritraverse f f f f quadruple

data Four a b c d
  = FourA !a
  | FourB !b
  | FourC !c
  | FourD !d

fourA (FourA a) = a
fourA _ = undefined

fourB (FourB b) = b
fourB _ = undefined

fourC (FourC c) = c
fourC _ = undefined

fourD (FourD d) = d
fourD _ = undefined

packQuadrupleM ::
  (Quadrifunctor f, Functor m) =>
  f (m a) (m b) (m c) (m d) ->
  Quadruple f (m (Four a b c d))
packQuadrupleM =
  Quadruple
    . quadrimap
      (fmap FourA)
      (fmap FourB)
      (fmap FourC)
      (fmap FourD)

unpackQuadruple ::
  (Quadrifunctor t) =>
  Quadruple t (Four a b c d) ->
  t a b c d
unpackQuadruple =
  quadrimap
    fourA
    fourB
    fourC
    fourD
    . runQuadruple

unpackQuadrupleM ::
  (Quadrifunctor f, Functor m) =>
  Quadruple f (m (Four a b c d)) ->
  f (m a) (m b) (m c) (m d)
unpackQuadrupleM =
  quadrimap
    (fmap fourA)
    (fmap fourB)
    (fmap fourC)
    (fmap fourD)
    . runQuadruple

goQuadruple cell = case cell of
  FourA run -> fmap FourA . run . unpackQuadrupleM
  FourB run -> fmap FourB . run . unpackQuadrupleM
  FourC run -> fmap FourC . run . unpackQuadrupleM
  FourD run -> fmap FourD . run . unpackQuadrupleM

newtype Loeb4 f s a b c d
  = Loeb4
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s a)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s b)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s c)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s d)
      )

loeb4 ::
  (Quadritraversable f) =>
  Loeb4 f s a b c d ->
  f a b c d
loeb4 (Loeb4 spreadsheet) = unpackQuadruple $ loeb $ Loeb $ fmap goQuadruple <$> packQuadrupleM spreadsheet

loebST4 ::
  (Quadritraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s d) ->
  ST s (t a b c d)
loebST4 spreadsheet = fmap unpackQuadruple $ loebST $ fmap goQuadruple <$> packQuadrupleM spreadsheet
