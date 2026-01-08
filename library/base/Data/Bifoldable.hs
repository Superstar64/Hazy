{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Bifoldable where

import Control.Applicative (Alternative)

class Bifoldable p where
  bifold :: (Monoid m) => p m m -> m
  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c

bifoldr' :: (Bifoldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' = error "todo"

bifoldr1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldr1 = error "todo"

bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM = error "todo"

bifoldl' :: (Bifoldable t) => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' = error "todo"

bifoldl1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldl1 = error "todo"

bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM = error "todo"

bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ = error "todo"

bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ = error "todo"

bimapM_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = error "todo"

biforM_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
biforM_ = error "todo"

bimsum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
bimsum = error "todo"

bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = error "todo"

bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = error "todo"

biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = error "todo"

biList :: (Bifoldable t) => t a a -> [a]
biList = error "todo"

binull :: (Bifoldable t) => t a b -> Bool
binull = error "todo"

bilength :: (Bifoldable t) => t a b -> Int
bilength = error "todo"

bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem = error "todo"

bimaximum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
bimaximum = error "todo"

biminimum :: forall t a. (Bifoldable t, Ord a) => t a a -> a
biminimum = error "todo"

bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = error "todo"

biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = error "todo"

biconcat :: (Bifoldable t) => t [a] [a] -> [a]
biconcat = error "todo"

biconcatMap :: (Bifoldable t) => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = error "todo"

biand :: (Bifoldable t) => t Bool Bool -> Bool
biand = error "todo"

bior :: (Bifoldable t) => t Bool Bool -> Bool
bior = error "todo"

biany :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany = error "todo"

biall :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall = error "todo"

bimaximumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
bimaximumBy = error "todo"

biminimumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
biminimumBy = error "todo"

binotElem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
binotElem = error "todo"

bifind :: (Bifoldable t) => (a -> Bool) -> t a a -> Maybe a
bifind = error "todo"
