module Data.Acyclic7
  ( Loeb7 (..),
    loeb7,
    loebST7,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Heptafoldable (Heptafoldable (..))
import Data.Heptafunctor (Heptafunctor (..))
import Data.Heptatraversable (Heptatraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Septuple f a = Septuple {runSeptuple :: f a a a a a a a}

instance (Heptafunctor f) => Functor (Septuple f) where
  fmap f (Septuple septuple) = Septuple (heptamap f f f f f f f septuple)

instance (Heptafoldable f) => Foldable (Septuple f) where
  foldMap f (Septuple septuple) = heptafoldMap f f f f f f f septuple

instance (Heptatraversable f) => Traversable (Septuple f) where
  traverse f (Septuple septuple) = Septuple <$> heptatraverse f f f f f f f septuple

data Seven a b c d e f g
  = SevenA !a
  | SevenB !b
  | SevenC !c
  | SevenD !d
  | SevenE !e
  | SevenF !f
  | SevenG !g

sevenA (SevenA a) = a
sevenA _ = undefined

sevenB (SevenB a) = a
sevenB _ = undefined

sevenC (SevenC a) = a
sevenC _ = undefined

sevenD (SevenD a) = a
sevenD _ = undefined

sevenE (SevenE a) = a
sevenE _ = undefined

sevenF (SevenF a) = a
sevenF _ = undefined

sevenG (SevenG a) = a
sevenG _ = undefined

packSeptupleM ::
  (Heptafunctor f', Functor m) =>
  f' (m a) (m b) (m c) (m d) (m e) (m f) (m g) ->
  Septuple f' (m (Seven a b c d e f g))
packSeptupleM =
  Septuple
    . heptamap
      (fmap SevenA)
      (fmap SevenB)
      (fmap SevenC)
      (fmap SevenD)
      (fmap SevenE)
      (fmap SevenF)
      (fmap SevenG)

unpackSeptuple ::
  (Heptafunctor t) =>
  Septuple t (Seven a b c d e f g) ->
  t a b c d e f g
unpackSeptuple =
  heptamap
    sevenA
    sevenB
    sevenC
    sevenD
    sevenE
    sevenF
    sevenG
    . runSeptuple

unpackSeptupleM ::
  (Heptafunctor f', Functor m) =>
  Septuple f' (m (Seven a b c d e f g)) ->
  f' (m a) (m b) (m c) (m d) (m e) (m f) (m g)
unpackSeptupleM =
  heptamap
    (fmap sevenA)
    (fmap sevenB)
    (fmap sevenC)
    (fmap sevenD)
    (fmap sevenE)
    (fmap sevenF)
    (fmap sevenG)
    . runSeptuple

goSeptuple cell = case cell of
  SevenA run -> fmap SevenA . run . unpackSeptupleM
  SevenB run -> fmap SevenB . run . unpackSeptupleM
  SevenC run -> fmap SevenC . run . unpackSeptupleM
  SevenD run -> fmap SevenD . run . unpackSeptupleM
  SevenE run -> fmap SevenE . run . unpackSeptupleM
  SevenF run -> fmap SevenF . run . unpackSeptupleM
  SevenG run -> fmap SevenG . run . unpackSeptupleM

newtype Loeb7 f' s a b c d e f g
  = Loeb7
      ( forall s.
        f'
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s a)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s b)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s c)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s d)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s e)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s f)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s g)
      )

loeb7 ::
  (Heptatraversable f') =>
  Loeb7 f' s a b c d e f g ->
  f' a b c d e f g
loeb7 (Loeb7 spreadsheet) = unpackSeptuple $ loeb $ Loeb $ fmap goSeptuple <$> packSeptupleM spreadsheet

loebST7 ::
  (Heptatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s e)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s f)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s g) ->
  ST s (t a b c d e f g)
loebST7 spreadsheet = fmap unpackSeptuple $ loebST $ fmap goSeptuple <$> packSeptupleM spreadsheet
