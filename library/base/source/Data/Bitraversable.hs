module Data.Bitraversable where

import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)

class (Bifunctor t, Bifoldable t) => Bitraversable t where
  bitraverse :: (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)

bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = error "todo"

bisequence :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequence = error "todo"

bimapM :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM = error "todo"

bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor = error "todo"

biforM :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
biforM = error "todo"

bimapAccumL :: (Bitraversable t) => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumL = error "todo"

bimapAccumR :: (Bitraversable t) => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumR = error "todo"

bimapDefault :: forall t a b c d. (Bitraversable t) => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault = error "todo"

bifoldMapDefault :: forall t m a b. (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault = error "todo"
