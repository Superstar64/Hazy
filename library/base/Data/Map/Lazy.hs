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

data Map k a

empty :: Map k a
empty = error "todo"

singleton :: k -> a -> Map k a
singleton = error "todo"

fromSet :: (k -> a) -> Set k -> Map k a
fromSet = error "todo"

fromArgSet :: Set (Arg k a) -> Map k a
fromArgSet = error "todo"

fromList :: (Ord k) => [(k, a)] -> Map k a
fromList = error "todo"

fromListWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith = error "todo"

fromListWithKey :: (Ord k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey = error "todo"

fromAscList :: (Eq k) => [(k, a)] -> Map k a
fromAscList = error "todo"

fromAscListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWith = error "todo"

fromAscListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWithKey = error "todo"

fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList = error "todo"

fromDescList :: (Eq k) => [(k, a)] -> Map k a
fromDescList = error "todo"

fromDescListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWith = error "todo"

fromDescListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromDescListWithKey = error "todo"

fromDistinctDescList :: [(k, a)] -> Map k a
fromDistinctDescList = error "todo"

insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert = error "todo"

insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = error "todo"

insertWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey = error "todo"

insertLookupWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey = error "todo"

delete :: (Ord k) => k -> Map k a -> Map k a
delete = error "todo"

adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjust = error "todo"

adjustWithKey :: (Ord k) => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey = error "todo"

update :: (Ord k) => (a -> Maybe a) -> k -> Map k a -> Map k a
update = error "todo"

updateWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey = error "todo"

updateLookupWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey = error "todo"

alter :: (Ord k) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = error "todo"

alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF = error "todo"

lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup = error "todo"

(!?) :: (Ord k) => Map k a -> k -> Maybe a
(!?) = error "todo"

(!) :: (Ord k) => Map k a -> k -> a
(!) = error "todo"

findWithDefault :: (Ord k) => a -> k -> Map k a -> a
findWithDefault = error "todo"

member :: (Ord k) => k -> Map k a -> Bool
member = error "todo"

notMember :: (Ord k) => k -> Map k a -> Bool
notMember = error "todo"

lookupLT :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupLT = error "todo"

lookupGT :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupGT = error "todo"

lookupLE :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupLE = error "todo"

lookupGE :: (Ord k) => k -> Map k v -> Maybe (k, v)
lookupGE = error "todo"

null :: Map k a -> Bool
null = error "todo"

size :: Map k a -> Int
size = error "todo"

union :: (Ord k) => Map k a -> Map k a -> Map k a
union = error "todo"

unionWith :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith = error "todo"

unionWithKey :: (Ord k) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey = error "todo"

unions :: (Foldable f, Ord k) => f (Map k a) -> Map k a
unions = error "todo"

unionsWith :: (Foldable f, Ord k) => (a -> a -> a) -> f (Map k a) -> Map k a
unionsWith = error "todo"

difference :: (Ord k) => Map k a -> Map k b -> Map k a
difference = error "todo"

(\\) :: (Ord k) => Map k a -> Map k b -> Map k a
(\\) = error "todo"

differenceWith :: (Ord k) => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith = error "todo"

differenceWithKey :: (Ord k) => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey = error "todo"

intersection :: (Ord k) => Map k a -> Map k b -> Map k a
intersection = error "todo"

intersectionWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith = error "todo"

intersectionWithKey :: (Ord k) => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey = error "todo"

disjoint :: (Ord k) => Map k a -> Map k b -> Bool
disjoint = error "todo"

compose :: (Ord b) => Map b c -> Map a b -> Map a c
compose = error "todo"

mergeWithKey ::
  (Ord k) =>
  (k -> a -> b -> Maybe c) ->
  (Map k a -> Map k c) ->
  (Map k b -> Map k c) ->
  Map k a ->
  Map k b ->
  Map k c
mergeWithKey = error "todo"

map :: (a -> b) -> Map k a -> Map k b
map = error "todo"

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey = error "todo"

traverseWithKey :: (Applicative t) => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey = error "todo"

traverseMaybeWithKey :: (Applicative f) => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseMaybeWithKey = error "todo"

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum = error "todo"

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey = error "todo"

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey = error "todo"

mapKeys :: (Ord k2) => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys = error "todo"

mapKeysWith :: (Ord k2) => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith = error "todo"

mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic = error "todo"

foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr = error "todo"

foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl = error "todo"

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey = error "todo"

foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey = error "todo"

foldMapWithKey :: (Monoid m) => (k -> a -> m) -> Map k a -> m
foldMapWithKey = error "todo"

foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' = error "todo"

foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldl' = error "todo"

foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' = error "todo"

foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' = error "todo"

elems :: Map k a -> [a]
elems = error "todo"

keys :: Map k a -> [k]
keys = error "todo"

assocs :: Map k a -> [(k, a)]
assocs = error "todo"

keysSet :: Map k a -> Set k
keysSet = error "todo"

argSet :: Map k a -> Set (Arg k a)
argSet = error "todo"

toList :: Map k a -> [(k, a)]
toList = error "todo"

toAscList :: Map k a -> [(k, a)]
toAscList = error "todo"

toDescList :: Map k a -> [(k, a)]
toDescList = error "todo"

filter :: (a -> Bool) -> Map k a -> Map k a
filter = error "todo"

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey = error "todo"

restrictKeys :: (Ord k) => Map k a -> Set k -> Map k a
restrictKeys = error "todo"

withoutKeys :: (Ord k) => Map k a -> Set k -> Map k a
withoutKeys = error "todo"

partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition = error "todo"

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey = error "todo"

takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone = error "todo"

dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone = error "todo"

spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
spanAntitone = error "todo"

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe = error "todo"

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey = error "todo"

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither = error "todo"

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey = error "todo"

split :: (Ord k) => k -> Map k a -> (Map k a, Map k a)
split = error "todo"

splitLookup :: (Ord k) => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup = error "todo"

splitRoot :: Map k b -> [Map k b]
splitRoot = error "todo"

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf = error "todo"

isSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy = error "todo"

isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf = error "todo"

isProperSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy = error "todo"

lookupIndex :: (Ord k) => k -> Map k a -> Maybe Int
lookupIndex = error "todo"

findIndex :: (Ord k) => k -> Map k a -> Int
findIndex = error "todo"

elemAt :: Int -> Map k a -> (k, a)
elemAt = error "todo"

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt = error "todo"

deleteAt :: Int -> Map k a -> Map k a
deleteAt = error "todo"

take :: Int -> Map k a -> Map k a
take = error "todo"

drop :: Int -> Map k a -> Map k a
drop = error "todo"

splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt = error "todo"

lookupMin :: Map k a -> Maybe (k, a)
lookupMin = error "todo"

lookupMax :: Map k a -> Maybe (k, a)
lookupMax = error "todo"

findMin :: Map k a -> (k, a)
findMin = error "todo"

findMax :: Map k a -> (k, a)
findMax = error "todo"

deleteMin :: Map k a -> Map k a
deleteMin = error "todo"

deleteMax :: Map k a -> Map k a
deleteMax = error "todo"

deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin = error "todo"

deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax = error "todo"

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin = error "todo"

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax = error "todo"

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey = error "todo"

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey = error "todo"

minView :: Map k a -> Maybe (a, Map k a)
minView = error "todo"

maxView :: Map k a -> Maybe (a, Map k a)
maxView = error "todo"

minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey = error "todo"

maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey = error "todo"

valid :: (Ord k) => Map k a -> Bool
valid = error "todo"
