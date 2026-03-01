module Data.Map.Lazy
  ( Map,
    empty,
    singleton,
    fromSet,
    fromArgSet,
    fromList,
    fromListWith,
    fromListWithKey,
    fromAscList,
    fromAscListWith,
    fromAscListWithKey,
    fromDistinctAscList,
    fromDescList,
    fromDescListWith,
    fromDescListWithKey,
    fromDistinctDescList,
    insert,
    insertWith,
    insertWithKey,
    insertLookupWithKey,
    delete,
    adjust,
    adjustWithKey,
    update,
    updateWithKey,
    updateLookupWithKey,
    alter,
    alterF,
    lookup,
    (!?),
    (!),
    findWithDefault,
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    size,
    union,
    unionWith,
    unionWithKey,
    unions,
    unionsWith,
    difference,
    (\\),
    differenceWith,
    differenceWithKey,
    intersection,
    intersectionWith,
    intersectionWithKey,
    disjoint,
    compose,
    mergeWithKey,
    map,
    mapWithKey,
    traverseWithKey,
    traverseMaybeWithKey,
    mapAccum,
    mapAccumWithKey,
    mapAccumRWithKey,
    mapKeys,
    mapKeysWith,
    mapKeysMonotonic,
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    foldMapWithKey,
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,
    elems,
    keys,
    assocs,
    keysSet,
    argSet,
    toList,
    toAscList,
    toDescList,
    filter,
    filterWithKey,
    restrictKeys,
    withoutKeys,
    partition,
    partitionWithKey,
    takeWhileAntitone,
    dropWhileAntitone,
    spanAntitone,
    mapMaybe,
    mapMaybeWithKey,
    mapEither,
    mapEitherWithKey,
    split,
    splitLookup,
    splitRoot,
    isSubmapOf,
    isSubmapOfBy,
    isProperSubmapOf,
    isProperSubmapOfBy,
    lookupIndex,
    findIndex,
    elemAt,
    updateAt,
    deleteAt,
    take,
    drop,
    splitAt,
    lookupMin,
    lookupMax,
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    deleteFindMin,
    deleteFindMax,
    updateMin,
    updateMax,
    updateMinWithKey,
    updateMaxWithKey,
    minView,
    maxView,
    minViewWithKey,
    maxViewWithKey,
    valid,
  )
where

import Data.Semigroup (Arg)
import Data.Set (Set)
import Hazy (placeholder)

data Map k a

empty :: Map k a
empty = placeholder

singleton :: k -> a -> Map k a
singleton = placeholder

fromSet :: (k -> a) -> Set k -> Map k a
fromSet = placeholder

fromArgSet :: Set (Arg k a) -> Map k a
fromArgSet = placeholder

fromList :: (Ord k) => [(k, a)] -> Map k a
fromList = placeholder

fromListWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith = placeholder

fromListWithKey :: (Ord k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey = placeholder

fromAscList :: (Eq k) => [(k, a)] -> Map k a
fromAscList = placeholder

fromAscListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWith = placeholder

fromAscListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWithKey = placeholder

fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList = placeholder

fromDescList :: (Eq k) => [(k, a)] -> Map k a
fromDescList = placeholder

fromDescListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWith = placeholder

fromDescListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWithKey = placeholder

fromDistinctDescList :: [(k, a)] -> Map k a
fromDistinctDescList = placeholder

insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert = placeholder

insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = placeholder

insertWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey = placeholder

insertLookupWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey = placeholder

delete :: (Ord k) => k -> Map k a -> Map k a
delete = placeholder

adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjust = placeholder

adjustWithKey :: (Ord k) => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey = placeholder

update :: (Ord k) => (a -> Maybe a) -> k -> Map k a -> Map k a
update = placeholder

updateWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey = placeholder

updateLookupWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey = placeholder

alter :: (Ord k) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = placeholder

alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF = placeholder

lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup = placeholder

(!?) :: (Ord k) => Map k a -> k -> Maybe a
(!?) = placeholder

(!) :: (Ord k) => Map k a -> k -> a
(!) = placeholder

findWithDefault :: (Ord k) => a -> k -> Map k a -> a
findWithDefault = placeholder

member :: (Ord k) => k -> Map k a -> Bool
member = placeholder

notMember :: (Ord k) => k -> Map k a -> Bool
notMember = placeholder

lookupLT :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupLT = placeholder

lookupGT :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupGT = placeholder

lookupLE :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupLE = placeholder

lookupGE :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupGE = placeholder

null :: Map k a -> Bool
null = placeholder

size :: Map k a -> Int
size = placeholder

union :: (Ord k) => Map k a -> Map k a -> Map k a
union = placeholder

unionWith :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith = placeholder

unionWithKey :: (Ord k) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey = placeholder

unions :: (Foldable f, Ord k) => f (Map k a) -> Map k a
unions = placeholder

unionsWith :: (Foldable f, Ord k) => (a -> a -> a) -> f (Map k a) -> Map k a
unionsWith = placeholder

difference :: (Ord k) => Map k a -> Map k b -> Map k a
difference = placeholder

(\\) :: (Ord k) => Map k a -> Map k b -> Map k a
(\\) = placeholder

differenceWith :: (Ord k) => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith = placeholder

differenceWithKey :: (Ord k) => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey = placeholder

intersection :: (Ord k) => Map k a -> Map k b -> Map k a
intersection = placeholder

intersectionWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith = placeholder

intersectionWithKey :: (Ord k) => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey = placeholder

disjoint :: (Ord k) => Map k a -> Map k b -> Bool
disjoint = placeholder

compose :: (Ord b) => Map b c -> Map a b -> Map a c
compose = placeholder

mergeWithKey ::
  (Ord k) =>
  (k -> a -> b -> Maybe c) ->
  (Map k a -> Map k c) ->
  (Map k b -> Map k c) ->
  Map k a ->
  Map k b ->
  Map k c
mergeWithKey = placeholder

map :: (a -> b) -> Map k a -> Map k b
map = placeholder

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey = placeholder

traverseWithKey :: (Applicative t) => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey = placeholder

traverseMaybeWithKey :: (Applicative f) => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseMaybeWithKey = placeholder

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum = placeholder

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey = placeholder

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey = placeholder

mapKeys :: (Ord k2) => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys = placeholder

mapKeysWith :: (Ord k2) => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith = placeholder

mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic = placeholder

foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr = placeholder

foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl = placeholder

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey = placeholder

foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey = placeholder

foldMapWithKey :: (Monoid m) => (k -> a -> m) -> Map k a -> m
foldMapWithKey = placeholder

foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' = placeholder

foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldl' = placeholder

foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' = placeholder

foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' = placeholder

elems :: Map k a -> [a]
elems = placeholder

keys :: Map k a -> [k]
keys = placeholder

assocs :: Map k a -> [(k, a)]
assocs = placeholder

keysSet :: Map k a -> Set k
keysSet = placeholder

argSet :: Map k a -> Set (Arg k a)
argSet = placeholder

toList :: Map k a -> [(k, a)]
toList = placeholder

toAscList :: Map k a -> [(k, a)]
toAscList = placeholder

toDescList :: Map k a -> [(k, a)]
toDescList = placeholder

filter :: (a -> Bool) -> Map k a -> Map k a
filter = placeholder

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey = placeholder

restrictKeys :: (Ord k) => Map k a -> Set k -> Map k a
restrictKeys = placeholder

withoutKeys :: (Ord k) => Map k a -> Set k -> Map k a
withoutKeys = placeholder

partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition = placeholder

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey = placeholder

takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone = placeholder

dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone = placeholder

spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
spanAntitone = placeholder

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe = placeholder

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey = placeholder

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither = placeholder

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey = placeholder

split :: (Ord k) => k -> Map k a -> (Map k a, Map k a)
split = placeholder

splitLookup :: (Ord k) => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup = placeholder

splitRoot :: Map k b -> [Map k b]
splitRoot = placeholder

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf = placeholder

isSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy = placeholder

isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf = placeholder

isProperSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy = placeholder

lookupIndex :: (Ord k) => k -> Map k a -> Maybe Int
lookupIndex = placeholder

findIndex :: (Ord k) => k -> Map k a -> Int
findIndex = placeholder

elemAt :: Int -> Map k a -> (k, a)
elemAt = placeholder

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt = placeholder

deleteAt :: Int -> Map k a -> Map k a
deleteAt = placeholder

take :: Int -> Map k a -> Map k a
take = placeholder

drop :: Int -> Map k a -> Map k a
drop = placeholder

splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt = placeholder

lookupMin :: Map k a -> Maybe (k, a)
lookupMin = placeholder

lookupMax :: Map k a -> Maybe (k, a)
lookupMax = placeholder

findMin :: Map k a -> (k, a)
findMin = placeholder

findMax :: Map k a -> (k, a)
findMax = placeholder

deleteMin :: Map k a -> Map k a
deleteMin = placeholder

deleteMax :: Map k a -> Map k a
deleteMax = placeholder

deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin = placeholder

deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax = placeholder

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin = placeholder

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax = placeholder

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey = placeholder

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey = placeholder

minView :: Map k a -> Maybe (a, Map k a)
minView = placeholder

maxView :: Map k a -> Maybe (a, Map k a)
maxView = placeholder

minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey = placeholder

maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey = placeholder

valid :: (Ord k) => Map k a -> Bool
valid = placeholder
