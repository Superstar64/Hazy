module Data.Acyclic5
  ( Loeb5 (..),
    loeb5,
    loebST5,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Pentafoldable (Pentafoldable (..))
import Data.Pentafunctor (Pentafunctor (..))
import Data.Pentatraversable (Pentatraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Quinetuple f a = Quinetuple {runQuinetuple :: f a a a a a}

instance (Pentafunctor f) => Functor (Quinetuple f) where
  fmap f (Quinetuple penta) = Quinetuple (pentamap f f f f f penta)

instance (Pentafoldable f) => Foldable (Quinetuple f) where
  foldMap f (Quinetuple penta) = pentafoldMap f f f f f penta

instance (Pentatraversable f) => Traversable (Quinetuple f) where
  traverse f (Quinetuple penta) = Quinetuple <$> pentatraverse f f f f f penta

data Five a b c d e
  = FiveA !a
  | FiveB !b
  | FiveC !c
  | FiveD !d
  | FiveE !e

fiveA (FiveA a) = a
fiveA _ = undefined

fiveB (FiveB b) = b
fiveB _ = undefined

fiveC (FiveC c) = c
fiveC _ = undefined

fiveD (FiveD d) = d
fiveD _ = undefined

fiveE (FiveE e) = e
fiveE _ = undefined

packQuintupleM ::
  (Pentafunctor f, Functor m) =>
  f (m a) (m b) (m c) (m d) (m e) ->
  Quinetuple f (m (Five a b c d e))
packQuintupleM =
  Quinetuple
    . pentamap
      (fmap FiveA)
      (fmap FiveB)
      (fmap FiveC)
      (fmap FiveD)
      (fmap FiveE)

unpackQuintuple ::
  (Pentafunctor t) =>
  Quinetuple t (Five a b c d e) ->
  t a b c d e
unpackQuintuple =
  pentamap
    fiveA
    fiveB
    fiveC
    fiveD
    fiveE
    . runQuinetuple

unpackQuintupleM ::
  (Pentafunctor f, Functor m) =>
  Quinetuple f (m (Five a b c d e)) ->
  f (m a) (m b) (m c) (m d) (m e)
unpackQuintupleM =
  pentamap
    (fmap fiveA)
    (fmap fiveB)
    (fmap fiveC)
    (fmap fiveD)
    (fmap fiveE)
    . runQuinetuple

goQuintuple cell = case cell of
  FiveA run -> fmap FiveA . run . unpackQuintupleM
  FiveB run -> fmap FiveB . run . unpackQuintupleM
  FiveC run -> fmap FiveC . run . unpackQuintupleM
  FiveD run -> fmap FiveD . run . unpackQuintupleM
  FiveE run -> fmap FiveE . run . unpackQuintupleM

newtype Loeb5 f s a b c d e
  = Loeb5
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s a)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s b)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s c)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s d)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s e)
      )

loeb5 ::
  (Pentatraversable f) =>
  Loeb5 f s a b c d e ->
  f a b c d e
loeb5 (Loeb5 spreadsheet) = unpackQuintuple $ loeb $ Loeb $ fmap goQuintuple <$> packQuintupleM spreadsheet

loebST5 ::
  (Pentatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s e) ->
  ST s (t a b c d e)
loebST5 spreadsheet = fmap unpackQuintuple $ loebST $ fmap goQuintuple <$> packQuintupleM spreadsheet
