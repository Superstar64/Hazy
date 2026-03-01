module Data.Bitraversable where

import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Hazy (placeholder)

class (Bifunctor t, Bifoldable t) => Bitraversable t where
  bitraverse :: (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)

bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = placeholder

bisequence :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequence = placeholder

bimapM :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM = placeholder

bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor = placeholder

biforM :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
biforM = placeholder

bimapAccumL :: (Bitraversable t) => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumL = placeholder

bimapAccumR :: (Bitraversable t) => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumR = placeholder

bimapDefault :: forall t a b c d. (Bitraversable t) => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault = placeholder

bifoldMapDefault :: forall t m a b. (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault = placeholder
