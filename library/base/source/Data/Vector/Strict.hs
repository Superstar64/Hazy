module Data.Vector.Strict where

import qualified Data.Vector as Lazy
import Hazy (placeholder)

data Vector a

length :: Vector a -> Int
length = placeholder

null :: Vector a -> Bool
null = placeholder

(!) :: Vector a -> Int -> a
(!) = placeholder

(!?) :: Vector a -> Int -> Maybe a
(!?) = placeholder

head :: Vector a -> a
head = placeholder

last :: Vector a -> a
last = placeholder

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = placeholder

unsafeHead :: Vector a -> a
unsafeHead = placeholder

unsafeLast :: Vector a -> a
unsafeLast = placeholder

indexM :: (Monad m) => Vector a -> Int -> m a
indexM = placeholder

headM :: (Monad m) => Vector a -> m a
headM = placeholder

lastM :: (Monad m) => Vector a -> m a
lastM = placeholder

unsafeIndexM :: (Monad m) => Vector a -> Int -> m a
unsafeIndexM = placeholder

unsafeHeadM :: (Monad m) => Vector a -> m a
unsafeHeadM = placeholder

unsafeLastM :: (Monad m) => Vector a -> m a
unsafeLastM = placeholder

slice :: Int -> Int -> Vector a -> Vector a
slice = placeholder

init :: Vector a -> Vector a
init = placeholder

tail :: Vector a -> Vector a
tail = placeholder

take :: Int -> Vector a -> Vector a
take = placeholder

drop :: Int -> Vector a -> Vector a
drop = placeholder

splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt = placeholder

uncons :: Vector a -> Maybe (a, Vector a)
uncons = placeholder

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc = placeholder

unsafeSlice :: Int -> Int -> Vector a -> Vector a
unsafeSlice = placeholder

unsafeInit :: Vector a -> Vector a
unsafeInit = placeholder

unsafeTail :: Vector a -> Vector a
unsafeTail = placeholder

unsafeTake :: Int -> Vector a -> Vector a
unsafeTake = placeholder

unsafeDrop :: Int -> Vector a -> Vector a
unsafeDrop = placeholder

empty :: Vector a
empty = placeholder

singleton :: a -> Vector a
singleton = placeholder

replicate :: Int -> a -> Vector a
replicate = placeholder

generate :: Int -> (Int -> a) -> Vector a
generate = placeholder

iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN = placeholder

replicateM :: (Monad m) => Int -> m a -> m (Vector a)
replicateM = placeholder

generateM :: (Monad m) => Int -> (Int -> m a) -> m (Vector a)
generateM = placeholder

iterateNM :: (Monad m) => Int -> (a -> m a) -> a -> m (Vector a)
iterateNM = placeholder

unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr = placeholder

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN = placeholder

unfoldrExactN :: Int -> (b -> (a, b)) -> b -> Vector a
unfoldrExactN = placeholder

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrM = placeholder

unfoldrNM :: (Monad m) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrNM = placeholder

unfoldrExactNM :: (Monad m) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
unfoldrExactNM = placeholder

constructN :: Int -> (Vector a -> a) -> Vector a
constructN = placeholder

constructrN :: Int -> (Vector a -> a) -> Vector a
constructrN = placeholder

enumFromN :: (Num a) => a -> Int -> Vector a
enumFromN = placeholder

enumFromStepN :: (Num a) => a -> a -> Int -> Vector a
enumFromStepN = placeholder

enumFromTo :: (Enum a) => a -> a -> Vector a
enumFromTo = placeholder

enumFromThenTo :: (Enum a) => a -> a -> a -> Vector a
enumFromThenTo = placeholder

cons :: a -> Vector a -> Vector a
cons = placeholder

snoc :: Vector a -> a -> Vector a
snoc = placeholder

(++) :: Vector a -> Vector a -> Vector a
(++) = placeholder

concat :: [Vector a] -> Vector a
concat = placeholder

force :: Vector a -> Vector a
force = placeholder

(//) :: Vector a -> [(Int, a)] -> Vector a
(//) = placeholder

update :: Vector a -> Vector (Int, a) -> Vector a
update = placeholder

update_ :: Vector a -> Vector Int -> Vector a -> Vector a
update_ = placeholder

unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
unsafeUpd = placeholder

unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
unsafeUpdate = placeholder

unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
unsafeUpdate_ = placeholder

accum :: (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
accum = placeholder

accumulate :: (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
accumulate = placeholder

accumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
accumulate_ = placeholder

unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
unsafeAccum = placeholder

unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
unsafeAccumulate = placeholder

unsafeAccumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
unsafeAccumulate_ = placeholder

reverse :: Vector a -> Vector a
reverse = placeholder

backpermute :: Vector a -> Vector Int -> Vector a
backpermute = placeholder

unsafeBackpermute :: Vector a -> Vector Int -> Vector a
unsafeBackpermute = placeholder

indexed :: Vector a -> Vector (Int, a)
indexed = placeholder

map :: (a -> b) -> Vector a -> Vector b
map = placeholder

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap = placeholder

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap = placeholder

mapM :: (Monad m) => (a -> m b) -> Vector a -> m (Vector b)
mapM = placeholder

imapM :: (Monad m) => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM = placeholder

mapM_ :: (Monad m) => (a -> m b) -> Vector a -> m ()
mapM_ = placeholder

imapM_ :: (Monad m) => (Int -> a -> m b) -> Vector a -> m ()
imapM_ = placeholder

forM :: (Monad m) => Vector a -> (a -> m b) -> m (Vector b)
forM = placeholder

forM_ :: (Monad m) => Vector a -> (a -> m b) -> m ()
forM_ = placeholder

iforM :: (Monad m) => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM = placeholder

iforM_ :: (Monad m) => Vector a -> (Int -> a -> m b) -> m ()
iforM_ = placeholder

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith = placeholder

zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 = placeholder

zipWith4 :: (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 = placeholder

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
zipWith5 = placeholder

zipWith6 ::
  (a -> b -> c -> d -> e -> f -> g) ->
  Vector a ->
  Vector b ->
  Vector c ->
  Vector d ->
  Vector e ->
  Vector f ->
  Vector g
zipWith6 = placeholder

izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
izipWith = placeholder

izipWith3 :: (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
izipWith3 = placeholder

izipWith4 :: (Int -> a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
izipWith4 = placeholder

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
izipWith5 = placeholder

izipWith6 ::
  (Int -> a -> b -> c -> d -> e -> f -> g) ->
  Vector a ->
  Vector b ->
  Vector c ->
  Vector d ->
  Vector e ->
  Vector f ->
  Vector g
izipWith6 = placeholder

zip :: Vector a -> Vector b -> Vector (a, b)
zip = placeholder

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = placeholder

zip4 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a, b, c, d)
zip4 = placeholder

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a, b, c, d, e)
zip5 = placeholder

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a, b, c, d, e, f)
zip6 = placeholder

zipWithM :: (Monad m) => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
zipWithM = placeholder

izipWithM :: (Monad m) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
izipWithM = placeholder

zipWithM_ :: (Monad m) => (a -> b -> m c) -> Vector a -> Vector b -> m ()
zipWithM_ = placeholder

izipWithM_ :: (Monad m) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
izipWithM_ = placeholder

unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip = placeholder

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 = placeholder

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 = placeholder

unzip5 :: Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 = placeholder

unzip6 :: Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
unzip6 = placeholder

filter :: (a -> Bool) -> Vector a -> Vector a
filter = placeholder

ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter = placeholder

filterM :: (Monad m) => (a -> m Bool) -> Vector a -> m (Vector a)
filterM = placeholder

uniq :: (Eq a) => Vector a -> Vector a
uniq = placeholder

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe = placeholder

imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe = placeholder

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM = placeholder

imapMaybeM :: (Monad m) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM = placeholder

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = placeholder

takeWhile :: (a -> Bool) -> Vector a -> Vector a
takeWhile = placeholder

dropWhile :: (a -> Bool) -> Vector a -> Vector a
dropWhile = placeholder

partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition = placeholder

unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
unstablePartition = placeholder

partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
partitionWith = placeholder

span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
span = placeholder

break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
break = placeholder

spanR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
spanR = placeholder

breakR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
breakR = placeholder

groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
groupBy = placeholder

group :: (Eq a) => Vector a -> [Vector a]
group = placeholder

elem :: (Eq a) => a -> Vector a -> Bool
elem = placeholder

notElem :: (Eq a) => a -> Vector a -> Bool
notElem = placeholder

find :: (a -> Bool) -> Vector a -> Maybe a
find = placeholder

findIndex :: (a -> Bool) -> Vector a -> Maybe Int
findIndex = placeholder

findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
findIndexR = placeholder

findIndices :: (a -> Bool) -> Vector a -> Vector Int
findIndices = placeholder

elemIndex :: (Eq a) => a -> Vector a -> Maybe Int
elemIndex = placeholder

elemIndices :: (Eq a) => a -> Vector a -> Vector Int
elemIndices = placeholder

foldl :: (a -> b -> a) -> a -> Vector b -> a
foldl = placeholder

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 = placeholder

foldl' :: (a -> b -> a) -> a -> Vector b -> a
foldl' = placeholder

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' = placeholder

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr = placeholder

foldr1 :: (a -> a -> a) -> Vector a -> a
foldr1 = placeholder

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' = placeholder

foldr1' :: (a -> a -> a) -> Vector a -> a
foldr1' = placeholder

ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl = placeholder

ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl' = placeholder

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr = placeholder

ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' = placeholder

foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap = placeholder

foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap' = placeholder

all :: (a -> Bool) -> Vector a -> Bool
all = placeholder

any :: (a -> Bool) -> Vector a -> Bool
any = placeholder

and :: Vector Bool -> Bool
and = placeholder

or :: Vector Bool -> Bool
or = placeholder

sum :: (Num a) => Vector a -> a
sum = placeholder

product :: (Num a) => Vector a -> a
product = placeholder

maximum :: (Ord a) => Vector a -> a
maximum = placeholder

maximumBy :: (a -> a -> Ordering) -> Vector a -> a
maximumBy = placeholder

maximumOn :: (Ord b) => (a -> b) -> Vector a -> a
maximumOn = placeholder

minimum :: (Ord a) => Vector a -> a
minimum = placeholder

minimumBy :: (a -> a -> Ordering) -> Vector a -> a
minimumBy = placeholder

minimumOn :: (Ord b) => (a -> b) -> Vector a -> a
minimumOn = placeholder

minIndex :: (Ord a) => Vector a -> Int
minIndex = placeholder

minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
minIndexBy = placeholder

maxIndex :: (Ord a) => Vector a -> Int
maxIndex = placeholder

maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
maxIndexBy = placeholder

foldM :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m a
foldM = placeholder

ifoldM :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM = placeholder

foldM' :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m a
foldM' = placeholder

ifoldM' :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM' = placeholder

fold1M :: (Monad m) => (a -> a -> m a) -> Vector a -> m a
fold1M = placeholder

fold1M' :: (Monad m) => (a -> a -> m a) -> Vector a -> m a
fold1M' = placeholder

foldM_ :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM_ = placeholder

ifoldM_ :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM_ = placeholder

foldM'_ :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM'_ = placeholder

ifoldM'_ :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM'_ = placeholder

fold1M_ :: (Monad m) => (a -> a -> m a) -> Vector a -> m ()
fold1M_ = placeholder

fold1M'_ :: (Monad m) => (a -> a -> m a) -> Vector a -> m ()
fold1M'_ = placeholder

sequence :: (Monad m) => Vector (m a) -> m (Vector a)
sequence = placeholder

sequence_ :: (Monad m) => Vector (m a) -> m ()
sequence_ = placeholder

prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl = placeholder

prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl' = placeholder

postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl = placeholder

postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl' = placeholder

scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl = placeholder

scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl' = placeholder

scanl1 :: (a -> a -> a) -> Vector a -> Vector a
scanl1 = placeholder

scanl1' :: (a -> a -> a) -> Vector a -> Vector a
scanl1' = placeholder

iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl = placeholder

iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl' = placeholder

prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr = placeholder

prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr' = placeholder

postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr = placeholder

postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr' = placeholder

scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr = placeholder

scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr' = placeholder

scanr1 :: (a -> a -> a) -> Vector a -> Vector a
scanr1 = placeholder

scanr1' :: (a -> a -> a) -> Vector a -> Vector a
scanr1' = placeholder

iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr = placeholder

iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr' = placeholder

eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
eqBy = placeholder

cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy = placeholder

toList :: Vector a -> [a]
toList = placeholder

fromList :: [a] -> Vector a
fromList = placeholder

fromListN :: Int -> [a] -> Vector a
fromListN = placeholder

toLazy :: Vector a -> Lazy.Vector a
toLazy = placeholder

fromLazy :: Lazy.Vector a -> Vector a
fromLazy = placeholder
