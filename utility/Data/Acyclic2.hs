module Data.Acyclic2
  ( Loeb2 (..),
    loeb2,
    loebST2,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic1 (Loeb (..), loeb, loebST)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Double f a = Double {runDouble :: f a a}

instance (Bifunctor f) => Functor (Double f) where
  fmap f (Double double) = Double (bimap f f double)

instance (Bifoldable f) => Foldable (Double f) where
  foldMap f (Double double) = bifoldMap f f double

instance (Bitraversable f) => Traversable (Double f) where
  traverse f (Double double) = Double <$> bitraverse f f double

data Two a b
  = TwoA !a
  | TwoB !b

twoA (TwoA a) = a
twoA _ = undefined

twoB (TwoB b) = b
twoB _ = undefined

packDoubleM :: (Bifunctor t, Functor f) => t (f a) (f b) -> Double t (f (Two a b))
packDoubleM = Double . bimap (fmap TwoA) (fmap TwoB)

unpackDouble :: (Bifunctor t) => Double t (Two a b) -> t a b
unpackDouble = bimap twoA twoB . runDouble

unpackDoubleM :: (Bifunctor f, Functor t) => Double f (t (Two a b)) -> f (t a) (t b)
unpackDoubleM = bimap (fmap twoA) (fmap twoB) . runDouble

goDouble cell = case cell of
  TwoA run -> fmap TwoA . run . unpackDoubleM
  TwoB run -> fmap TwoB . run . unpackDoubleM

newtype Loeb2 f s a b
  = Loeb2
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) -> ST s a)
          (Void, f (ST s a) (ST s b) -> ST s b)
      )

loeb2 ::
  (Bitraversable f) =>
  Loeb2 f s a b ->
  f a b
loeb2 (Loeb2 spreadsheet) = unpackDouble $ loeb $ Loeb $ fmap goDouble <$> packDoubleM spreadsheet

loebST2 ::
  (Bitraversable t) =>
  t
    (Void, t (ST s a) (ST s b) -> ST s a)
    (Void, t (ST s a) (ST s b) -> ST s b) ->
  ST s (t a b)
loebST2 spreadsheet = fmap unpackDouble $ loebST $ fmap goDouble <$> packDoubleM spreadsheet
