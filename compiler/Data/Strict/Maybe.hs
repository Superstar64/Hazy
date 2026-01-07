module Data.Strict.Maybe where

import Prelude hiding (Maybe (Just, Nothing))

data Maybe e
  = Nothing
  | Just !e
  deriving (Show, Eq)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Foldable Maybe where
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = Just <$> f x

isNothing Nothing = True
isNothing _ = False

isJust Nothing = False
isJust _ = True
