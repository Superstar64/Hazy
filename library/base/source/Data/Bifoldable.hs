module Data.Bifoldable where

import Control.Applicative (Alternative)
import Hazy.Prelude (placeholder)

class Bifoldable p where
  bifold :: (Monoid m) => p m m -> m
  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c

bifoldr' :: (Bifoldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' = placeholder

bifoldr1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldr1 = placeholder

bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM = placeholder

bifoldl' :: (Bifoldable t) => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' = placeholder

bifoldl1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldl1 = placeholder

bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM = placeholder

bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ = placeholder

bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ = placeholder

bimapM_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = placeholder

biforM_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
biforM_ = placeholder

bimsum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
bimsum = placeholder

bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = placeholder

bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = placeholder

biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = placeholder

biList :: (Bifoldable t) => t a a -> [a]
biList = placeholder

binull :: (Bifoldable t) => t a b -> Bool
binull = placeholder

bilength :: (Bifoldable t) => t a b -> Int
bilength = placeholder

bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem = placeholder

bimaximum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
bimaximum = placeholder

biminimum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
biminimum = placeholder

bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = placeholder

biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = placeholder

biconcat :: (Bifoldable t) => t [a] [a] -> [a]
biconcat = placeholder

biconcatMap :: (Bifoldable t) => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = placeholder

biand :: (Bifoldable t) => t Bool Bool -> Bool
biand = placeholder

bior :: (Bifoldable t) => t Bool Bool -> Bool
bior = placeholder

biany :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany = placeholder

biall :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall = placeholder

bimaximumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
bimaximumBy = placeholder

biminimumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
biminimumBy = placeholder

binotElem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
binotElem = placeholder

bifind :: (Bifoldable t) => (a -> Bool) -> t a a -> Maybe a
bifind = placeholder
