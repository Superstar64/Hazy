module Data.Acyclic6
  ( Loeb6 (..),
    loeb6,
    loebST6,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Hexafoldable (Hexafoldable (..))
import Data.Hexafunctor (Hexafunctor (..))
import Data.Hexatraversable (Hexatraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Sixtuple f a = Sixtuple {runSixtuple :: f a a a a a a}

instance (Hexafunctor f) => Functor (Sixtuple f) where
  fmap f (Sixtuple sixtuple) = Sixtuple (hexamap f f f f f f sixtuple)

instance (Hexafoldable f) => Foldable (Sixtuple f) where
  foldMap f (Sixtuple sixtuple) = hexafoldMap f f f f f f sixtuple

instance (Hexatraversable f) => Traversable (Sixtuple f) where
  traverse f (Sixtuple sixtuple) = Sixtuple <$> hexatraverse f f f f f f sixtuple

data Six a b c d e f
  = SixA !a
  | SixB !b
  | SixC !c
  | SixD !d
  | SixE !e
  | SixF !f

sixA (SixA a) = a
sixA _ = undefined

sixB (SixB a) = a
sixB _ = undefined

sixC (SixC a) = a
sixC _ = undefined

sixD (SixD a) = a
sixD _ = undefined

sixE (SixE a) = a
sixE _ = undefined

sixF (SixF a) = a
sixF _ = undefined

packSixtupleM ::
  (Hexafunctor f', Functor m) =>
  f' (m a) (m b) (m c) (m d) (m e) (m f) ->
  Sixtuple f' (m (Six a b c d e f))
packSixtupleM =
  Sixtuple
    . hexamap
      (fmap SixA)
      (fmap SixB)
      (fmap SixC)
      (fmap SixD)
      (fmap SixE)
      (fmap SixF)

unpackSixtuple ::
  (Hexafunctor t) =>
  Sixtuple t (Six a b c d e f) ->
  t a b c d e f
unpackSixtuple =
  hexamap
    sixA
    sixB
    sixC
    sixD
    sixE
    sixF
    . runSixtuple

unpackSixtupleM ::
  (Hexafunctor f', Functor m) =>
  Sixtuple f' (m (Six a b c d e f)) ->
  f' (m a) (m b) (m c) (m d) (m e) (m f)
unpackSixtupleM =
  hexamap
    (fmap sixA)
    (fmap sixB)
    (fmap sixC)
    (fmap sixD)
    (fmap sixE)
    (fmap sixF)
    . runSixtuple

goSixtuple cell = case cell of
  SixA run -> fmap SixA . run . unpackSixtupleM
  SixB run -> fmap SixB . run . unpackSixtupleM
  SixC run -> fmap SixC . run . unpackSixtupleM
  SixD run -> fmap SixD . run . unpackSixtupleM
  SixE run -> fmap SixE . run . unpackSixtupleM
  SixF run -> fmap SixF . run . unpackSixtupleM

newtype Loeb6 f' s a b c d e f
  = Loeb6
      ( forall s.
        f'
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s a)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s b)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s c)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s d)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s e)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s f)
      )

loeb6 ::
  (Hexatraversable f') =>
  Loeb6 f' s a b c d e f ->
  f' a b c d e f
loeb6 (Loeb6 spreadsheet) = unpackSixtuple $ loeb $ Loeb $ fmap goSixtuple <$> packSixtupleM spreadsheet

loebST6 ::
  (Hexatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s e)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s f) ->
  ST s (t a b c d e f)
loebST6 spreadsheet = fmap unpackSixtuple $ loebST $ fmap goSixtuple <$> packSixtupleM spreadsheet
