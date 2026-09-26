module Data.Strict.Vector2 (Vector2, fromList'', toVector, unzip) where

import Data.Traversable (fmapDefault, foldMapDefault)
import qualified Data.Vector.Strict as Data.Strict (Vector)
import qualified Data.Vector.Strict as Data.Strict.Vector
import Prelude hiding (unzip)

newtype Vector2 a = Vector2 (Data.Strict.Vector a)
  deriving (Eq, Ord, Show)

fromList'' :: a -> a -> [a] -> Vector2 a
fromList'' head1 head2 tail = Vector2 (Data.Strict.Vector.fromList (head1 : head2 : tail))

instance Functor Vector2 where
  fmap = fmapDefault

instance Foldable Vector2 where
  foldMap = foldMapDefault
  length (Vector2 elements) = length elements

instance Traversable Vector2 where
  traverse f (Vector2 vector) = Vector2 <$> traverse f vector

toVector :: Vector2 a -> Data.Strict.Vector.Vector a
toVector (Vector2 vector) = vector

unzip :: Vector2 (a, b) -> (Vector2 a, Vector2 b)
unzip (Vector2 vector) =
  let (left, right) = Data.Strict.Vector.unzip vector
   in (Vector2 left, Vector2 right)
