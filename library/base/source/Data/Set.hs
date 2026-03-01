module Data.Set
  ( Set,
    empty,
    singleton,
    fromList,
    fromAscList,
    fromDescList,
    fromDistinctAscList,
    fromDistinctDescList,
    powerSet,
    insert,
    delete,
    alterF,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    size,
    isSubsetOf,
    isProperSubsetOf,
    disjoint,
    union,
    unions,
    difference,
    (\\),
    intersection,
    cartesianProduct,
    disjointUnion,
    filter,
    takeWhileAntitone,
    dropWhileAntitone,
    spanAntitone,
    partition,
    split,
    splitMember,
    splitRoot,
    lookupIndex,
    findIndex,
    elemAt,
    deleteAt,
    take,
    drop,
    splitAt,
    map,
    mapMonotonic,
    foldr,
    foldl,
    foldr,
    foldl,
    fold,
    lookupMin,
    lookupMax,
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    deleteFindMin,
    deleteFindMax,
    maxView,
    minView,
    elems,
    toList,
    toAscList,
    toDescList,
    showTree,
    showTreeWith,
    valid,
  )
where

import Hazy (placeholder)

data Set a

empty :: Set a
empty = placeholder

singleton :: a -> Set a
singleton = placeholder

fromList :: (Ord a) => [a] -> Set a
fromList = placeholder

fromAscList :: (Eq a) => [a] -> Set a
fromAscList = placeholder

fromDescList :: (Eq a) => [a] -> Set a
fromDescList = placeholder

fromDistinctAscList :: [a] -> Set a
fromDistinctAscList = placeholder

fromDistinctDescList :: [a] -> Set a
fromDistinctDescList = placeholder

powerSet :: Set a -> Set (Set a)
powerSet = placeholder

insert :: (Ord a) => a -> Set a -> Set a
insert = placeholder

delete :: (Ord a) => a -> Set a -> Set a
delete = placeholder

alterF :: (Ord a, Functor f) => (Bool -> f Bool) -> a -> Set a -> f (Set a)
alterF = placeholder

member :: (Ord a) => a -> Set a -> Bool
member = placeholder

notMember :: (Ord a) => a -> Set a -> Bool
notMember = placeholder

lookupLT :: (Ord a) => a -> Set a -> Maybe a
lookupLT = placeholder

lookupGT :: (Ord a) => a -> Set a -> Maybe a
lookupGT = placeholder

lookupLE :: (Ord a) => a -> Set a -> Maybe a
lookupLE = placeholder

lookupGE :: (Ord a) => a -> Set a -> Maybe a
lookupGE = placeholder

null :: Set a -> Bool
null = placeholder

size :: Set a -> Int
size = placeholder

isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf = placeholder

isProperSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isProperSubsetOf = placeholder

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint = placeholder

union :: (Ord a) => Set a -> Set a -> Set a
union = placeholder

unions :: (Foldable f, Ord a) => f (Set a) -> Set a
unions = placeholder

difference :: (Ord a) => Set a -> Set a -> Set a
difference = placeholder

(\\) :: (Ord a) => Set a -> Set a -> Set a
(\\) = placeholder

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection = placeholder

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = placeholder

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = placeholder

filter :: (a -> Bool) -> Set a -> Set a
filter = placeholder

takeWhileAntitone :: (a -> Bool) -> Set a -> Set a
takeWhileAntitone = placeholder

dropWhileAntitone :: (a -> Bool) -> Set a -> Set a
dropWhileAntitone = placeholder

spanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
spanAntitone = placeholder

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = placeholder

split :: (Ord a) => a -> Set a -> (Set a, Set a)
split = placeholder

splitMember :: (Ord a) => a -> Set a -> (Set a, Bool, Set a)
splitMember = placeholder

splitRoot :: Set a -> [Set a]
splitRoot = placeholder

lookupIndex :: (Ord a) => a -> Set a -> Maybe Int
lookupIndex = placeholder

findIndex :: (Ord a) => a -> Set a -> Int
findIndex = placeholder

elemAt :: Int -> Set a -> a
elemAt = placeholder

deleteAt :: Int -> Set a -> Set a
deleteAt = placeholder

take :: Int -> Set a -> Set a
take = placeholder

drop :: Int -> Set a -> Set a
drop = placeholder

splitAt :: Int -> Set a -> (Set a, Set a)
splitAt = placeholder

map :: (Ord b) => (a -> b) -> Set a -> Set b
map = placeholder

mapMonotonic :: (a -> b) -> Set a -> Set b
mapMonotonic = placeholder

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr = placeholder

foldl :: (a -> b -> a) -> a -> Set b -> a
foldl = placeholder

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' = placeholder

foldl' :: (a -> b -> a) -> a -> Set b -> a
foldl' = placeholder

fold :: (a -> b -> b) -> b -> Set a -> b
fold = placeholder

lookupMin :: Set a -> Maybe a
lookupMin = placeholder

lookupMax :: Set a -> Maybe a
lookupMax = placeholder

findMin :: Set a -> a
findMin = placeholder

findMax :: Set a -> a
findMax = placeholder

deleteMin :: Set a -> Set a
deleteMin = placeholder

deleteMax :: Set a -> Set a
deleteMax = placeholder

deleteFindMin :: Set a -> (a, Set a)
deleteFindMin = placeholder

deleteFindMax :: Set a -> (a, Set a)
deleteFindMax = placeholder

maxView :: Set a -> Maybe (a, Set a)
maxView = placeholder

minView :: Set a -> Maybe (a, Set a)
minView = placeholder

elems :: Set a -> [a]
elems = placeholder

toList :: Set a -> [a]
toList = placeholder

toAscList :: Set a -> [a]
toAscList = placeholder

toDescList :: Set a -> [a]
toDescList = placeholder

showTree :: (Show a) => Set a -> String
showTree = placeholder

showTreeWith :: (Show a) => Bool -> Bool -> Set a -> String
showTreeWith = placeholder

valid :: (Ord a) => Set a -> Bool
valid = placeholder
