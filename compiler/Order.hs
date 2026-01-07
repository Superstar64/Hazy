module Order
  ( orderListInt,
    orderListInt',
    orderNonEmpty,
    orderNonEmpty',
    orderList,
    orderList',
    orderWithInt,
    orderWithInt',
    orderWith,
    orderWith',
    order,
    order',
    orderInt,
    orderInt',
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector

orderListInt :: ([v] -> a) -> Int -> [(Int, v)] -> Vector a
orderListInt combine length source = Vector.generate length (combine . (combined Vector.!))
  where
    base = Vector.replicate length []
    combined = Vector.map reverse $ Vector.accum (flip (:)) base source

orderListInt' :: ([v] -> a) -> Int -> [(Int, v)] -> Strict.Vector a
orderListInt' combine length source = Strict.Vector.fromLazy $ orderListInt combine length source

orderNonEmpty :: (Ord k) => (NonEmpty v -> b) -> [(k, v)] -> Vector b
orderNonEmpty combine source = orderListInt (combine . NonEmpty.fromList) (length unique) indexed
  where
    unique = nubOrd (map fst source)
    order = Map.fromList $ [(key, index) | (index, key) <- zip [0 ..] unique]
    indexed = [(order Map.! key, value) | (key, value) <- source]

orderNonEmpty' :: (Ord k) => (NonEmpty v -> a) -> [(k, v)] -> Strict.Vector a
orderNonEmpty' combine source = Strict.Vector.fromLazy $ orderNonEmpty combine source

orderList :: (Ord k) => ([v] -> b) -> [(k, v)] -> Vector b
orderList combine = orderNonEmpty (combine . NonEmpty.toList)

orderList' :: (Ord k) => ([v] -> a) -> [(k, v)] -> Strict.Vector a
orderList' combine source = Strict.Vector.fromLazy $ orderList combine source

orderWith :: (Ord k) => (v -> v -> v) -> [(k, v)] -> Vector v
orderWith combine = orderList (foldr1 combine)

orderWith' :: (Ord k) => (a -> a -> a) -> [(k, a)] -> Strict.Vector a
orderWith' combine source = Strict.Vector.fromLazy $ orderWith combine source

orderWithInt :: (v -> v -> v) -> v -> Int -> [(Int, v)] -> Vector v
orderWithInt combine seed = orderListInt (foldr combine seed)

orderWithInt' :: (v -> v -> v) -> v -> Int -> [(Int, v)] -> Strict.Vector v
orderWithInt' combine seed length source = Strict.Vector.fromLazy $ orderWithInt combine seed length source

order :: (Ord k, Semigroup v) => [(k, v)] -> Vector v
order = orderWith (<>)

order' :: (Ord k, Semigroup a) => [(k, a)] -> Strict.Vector a
order' source = Strict.Vector.fromLazy $ order source

orderInt :: (Monoid v) => Int -> [(Int, v)] -> Vector v
orderInt = orderWithInt (<>) mempty

orderInt' :: (Monoid v) => Int -> [(Int, v)] -> Strict.Vector v
orderInt' length source = Strict.Vector.fromLazy $ orderInt length source
