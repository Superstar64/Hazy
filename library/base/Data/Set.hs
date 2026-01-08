{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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

data Set a

empty :: Set a
empty = error "todo"

singleton :: a -> Set a
singleton = error "todo"

fromList :: (Ord a) => [a] -> Set a
fromList = error "todo"

fromAscList :: (Eq a) => [a] -> Set a
fromAscList = error "todo"

fromDescList :: (Eq a) => [a] -> Set a
fromDescList = error "todo"

fromDistinctAscList :: [a] -> Set a
fromDistinctAscList = error "todo"

fromDistinctDescList :: [a] -> Set a
fromDistinctDescList = error "todo"

powerSet :: Set a -> Set (Set a)
powerSet = error "todo"

insert :: (Ord a) => a -> Set a -> Set a
insert = error "todo"

delete :: (Ord a) => a -> Set a -> Set a
delete = error "todo"

alterF :: (Ord a, Functor f) => (Bool -> f Bool) -> a -> Set a -> f (Set a)
alterF = error "todo"

member :: (Ord a) => a -> Set a -> Bool
member = error "todo"

notMember :: (Ord a) => a -> Set a -> Bool
notMember = error "todo"

lookupLT :: (Ord a) => a -> Set a -> Maybe a
lookupLT = error "todo"

lookupGT :: (Ord a) => a -> Set a -> Maybe a
lookupGT = error "todo"

lookupLE :: (Ord a) => a -> Set a -> Maybe a
lookupLE = error "todo"

lookupGE :: (Ord a) => a -> Set a -> Maybe a
lookupGE = error "todo"

null :: Set a -> Bool
null = error "todo"

size :: Set a -> Int
size = error "todo"

isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf = error "todo"

isProperSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isProperSubsetOf = error "todo"

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint = error "todo"

union :: (Ord a) => Set a -> Set a -> Set a
union = error "todo"

unions :: (Foldable f, Ord a) => f (Set a) -> Set a
unions = error "todo"

difference :: (Ord a) => Set a -> Set a -> Set a
difference = error "todo"

(\\) :: (Ord a) => Set a -> Set a -> Set a
(\\) = error "todo"

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection = error "todo"

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = error "todo"

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = error "todo"

filter :: (a -> Bool) -> Set a -> Set a
filter = error "todo"

takeWhileAntitone :: (a -> Bool) -> Set a -> Set a
takeWhileAntitone = error "todo"

dropWhileAntitone :: (a -> Bool) -> Set a -> Set a
dropWhileAntitone = error "todo"

spanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
spanAntitone = error "todo"

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = error "todo"

split :: (Ord a) => a -> Set a -> (Set a, Set a)
split = error "todo"

splitMember :: (Ord a) => a -> Set a -> (Set a, Bool, Set a)
splitMember = error "todo"

splitRoot :: Set a -> [Set a]
splitRoot = error "todo"

lookupIndex :: (Ord a) => a -> Set a -> Maybe Int
lookupIndex = error "todo"

findIndex :: (Ord a) => a -> Set a -> Int
findIndex = error "todo"

elemAt :: Int -> Set a -> a
elemAt = error "todo"

deleteAt :: Int -> Set a -> Set a
deleteAt = error "todo"

take :: Int -> Set a -> Set a
take = error "todo"

drop :: Int -> Set a -> Set a
drop = error "todo"

splitAt :: Int -> Set a -> (Set a, Set a)
splitAt = error "todo"

map :: (Ord b) => (a -> b) -> Set a -> Set b
map = error "todo"

mapMonotonic :: (a -> b) -> Set a -> Set b
mapMonotonic = error "todo"

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr = error "todo"

foldl :: (a -> b -> a) -> a -> Set b -> a
foldl = error "todo"

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' = error "todo"

foldl' :: (a -> b -> a) -> a -> Set b -> a
foldl' = error "todo"

fold :: (a -> b -> b) -> b -> Set a -> b
fold = error "todo"

lookupMin :: Set a -> Maybe a
lookupMin = error "todo"

lookupMax :: Set a -> Maybe a
lookupMax = error "todo"

findMin :: Set a -> a
findMin = error "todo"

findMax :: Set a -> a
findMax = error "todo"

deleteMin :: Set a -> Set a
deleteMin = error "todo"

deleteMax :: Set a -> Set a
deleteMax = error "todo"

deleteFindMin :: Set a -> (a, Set a)
deleteFindMin = error "todo"

deleteFindMax :: Set a -> (a, Set a)
deleteFindMax = error "todo"

maxView :: Set a -> Maybe (a, Set a)
maxView = error "todo"

minView :: Set a -> Maybe (a, Set a)
minView = error "todo"

elems :: Set a -> [a]
elems = error "todo"

toList :: Set a -> [a]
toList = error "todo"

toAscList :: Set a -> [a]
toAscList = error "todo"

toDescList :: Set a -> [a]
toDescList = error "todo"

showTree :: (Show a) => Set a -> String
showTree = error "todo"

showTreeWith :: (Show a) => Bool -> Bool -> Set a -> String
showTreeWith = error "todo"

valid :: (Ord a) => Set a -> Bool
valid = error "todo"
