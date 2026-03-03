module Data.Acyclic8
  ( Loeb8 (..),
    loeb8,
    loebST8,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Octafoldable (Octafoldable (..))
import Data.Octafunctor (Octafunctor (..))
import Data.Octatraversable (Octatraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Octuple f a = Octuple {runOctuple :: f a a a a a a a a}

instance (Octafunctor f) => Functor (Octuple f) where
  fmap f (Octuple oct) = Octuple (octamap f f f f f f f f oct)

instance (Octafoldable f) => Foldable (Octuple f) where
  foldMap f (Octuple oct) = octafoldMap f f f f f f f f oct

instance (Octatraversable f) => Traversable (Octuple f) where
  traverse f (Octuple oct) = Octuple <$> octatraverse f f f f f f f f oct

data Eight a b c d e f g h
  = EightA !a
  | EightB !b
  | EightC !c
  | EightD !d
  | EightE !e
  | EightF !f
  | EightG !g
  | EightH !h

eightA (EightA a) = a
eightA _ = undefined

eightB (EightB a) = a
eightB _ = undefined

eightC (EightC a) = a
eightC _ = undefined

eightD (EightD a) = a
eightD _ = undefined

eightE (EightE a) = a
eightE _ = undefined

eightF (EightF a) = a
eightF _ = undefined

eightG (EightG a) = a
eightG _ = undefined

eightH (EightH a) = a
eightH _ = undefined

packOctupleM ::
  (Octafunctor f', Functor m) =>
  f' (m a) (m b) (m c) (m d) (m e) (m f) (m g) (m h) -> Octuple f' (m (Eight a b c d e f g h))
packOctupleM =
  Octuple
    . octamap
      (fmap EightA)
      (fmap EightB)
      (fmap EightC)
      (fmap EightD)
      (fmap EightE)
      (fmap EightF)
      (fmap EightG)
      (fmap EightH)

unpackOctuple :: (Octafunctor t) => Octuple t (Eight a b c d e f g h) -> t a b c d e f g h
unpackOctuple = octamap eightA eightB eightC eightD eightE eightF eightG eightH . runOctuple

unpackOctupleM ::
  (Octafunctor f', Functor m) =>
  Octuple f' (m (Eight a b c d e f g h)) -> f' (m a) (m b) (m c) (m d) (m e) (m f) (m g) (m h)
unpackOctupleM =
  octamap
    (fmap eightA)
    (fmap eightB)
    (fmap eightC)
    (fmap eightD)
    (fmap eightE)
    (fmap eightF)
    (fmap eightG)
    (fmap eightH)
    . runOctuple

goOctuple cell = case cell of
  EightA run -> fmap EightA . run . unpackOctupleM
  EightB run -> fmap EightB . run . unpackOctupleM
  EightC run -> fmap EightC . run . unpackOctupleM
  EightD run -> fmap EightD . run . unpackOctupleM
  EightE run -> fmap EightE . run . unpackOctupleM
  EightF run -> fmap EightF . run . unpackOctupleM
  EightG run -> fmap EightG . run . unpackOctupleM
  EightH run -> fmap EightH . run . unpackOctupleM

newtype Loeb8 f' s a b c d e f g h
  = Loeb8
      ( forall s.
        f'
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s a)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s b)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s c)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s d)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s e)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s f)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s g)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s h)
      )

loeb8 :: (Octatraversable f') => Loeb8 f' s a b c d e f g h -> f' a b c d e f g h
loeb8 (Loeb8 spreadsheet) = unpackOctuple $ loeb $ Loeb $ fmap goOctuple <$> packOctupleM spreadsheet

loebST8 ::
  (Octatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s e)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s f)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s g)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s h) ->
  ST s (t a b c d e f g h)
loebST8 spreadsheet = fmap unpackOctuple $ loebST $ fmap goOctuple <$> packOctupleM spreadsheet
