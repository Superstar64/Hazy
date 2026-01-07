module Data.Strict.Vector1
  ( Vector1,
    fromList',
    fromNonEmpty,
    fromVector,
    toVector,
    singleton,
    uncons,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Traversable (fmapDefault, foldMapDefault)
import qualified Data.Vector.Strict as Data.Strict (Vector)
import qualified Data.Vector.Strict as Data.Strict.Vector

newtype Vector1 a = Vector1 (Data.Strict.Vector a)
  deriving (Eq, Ord, Show)

instance Functor Vector1 where
  fmap = fmapDefault

instance Foldable Vector1 where
  foldMap = foldMapDefault

instance Traversable Vector1 where
  traverse f (Vector1 vector) = seqVector <$> traverse f vector
    where
      seqVector vector | () <- foldr seq () vector = Vector1 vector

instance Semigroup (Vector1 a) where
  Vector1 vector1 <> Vector1 vector2 = Vector1 (vector1 <> vector2)

fromList' :: a -> [a] -> Vector1 a
fromList' head tail = Vector1 (Data.Strict.Vector.fromList (head : tail))

fromNonEmpty :: NonEmpty.NonEmpty a -> Vector1 a
fromNonEmpty (head NonEmpty.:| tail) = fromList' head tail

fromVector :: Data.Strict.Vector a -> Vector1 a
fromVector vector
  | not (null vector) = Vector1 vector
  | otherwise = error "bad nonempty vector"

toVector :: Vector1 a -> Data.Strict.Vector a
toVector (Vector1 vector) = vector

singleton :: a -> Vector1 a
singleton = Vector1 . Data.Strict.Vector.singleton

uncons :: Vector1 a -> (a, Data.Strict.Vector a)
uncons (Vector1 vector) = fromJust $ Data.Strict.Vector.uncons vector
