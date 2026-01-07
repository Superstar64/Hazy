module Data.Vector.Strict where

import qualified Data.Vector as Lazy

data Vector a

length :: Vector a -> Int
length = error "todo"

null :: Vector a -> Bool
null = error "todo"

(!) :: Vector a -> Int -> a
(!) = error "todo"

(!?) :: Vector a -> Int -> Maybe a
(!?) = error "todo"

head :: Vector a -> a
head = error "todo"

last :: Vector a -> a
last = error "todo"

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = error "todo"

unsafeHead :: Vector a -> a
unsafeHead = error "todo"

unsafeLast :: Vector a -> a
unsafeLast = error "todo"

indexM :: (Monad m) => Vector a -> Int -> m a
indexM = error "todo"

headM :: (Monad m) => Vector a -> m a
headM = error "todo"

lastM :: (Monad m) => Vector a -> m a
lastM = error "todo"

unsafeIndexM :: (Monad m) => Vector a -> Int -> m a
unsafeIndexM = error "todo"

unsafeHeadM :: (Monad m) => Vector a -> m a
unsafeHeadM = error "todo"

unsafeLastM :: (Monad m) => Vector a -> m a
unsafeLastM = error "todo"

slice :: Int -> Int -> Vector a -> Vector a
slice = error "todo"

init :: Vector a -> Vector a
init = error "todo"

tail :: Vector a -> Vector a
tail = error "todo"

take :: Int -> Vector a -> Vector a
take = error "todo"

drop :: Int -> Vector a -> Vector a
drop = error "todo"

splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt = error "todo"

uncons :: Vector a -> Maybe (a, Vector a)
uncons = error "todo"

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc = error "todo"

unsafeSlice :: Int -> Int -> Vector a -> Vector a
unsafeSlice = error "todo"

unsafeInit :: Vector a -> Vector a
unsafeInit = error "todo"

unsafeTail :: Vector a -> Vector a
unsafeTail = error "todo"

unsafeTake :: Int -> Vector a -> Vector a
unsafeTake = error "todo"

unsafeDrop :: Int -> Vector a -> Vector a
unsafeDrop = error "todo"

empty :: Vector a
empty = error "todo"

singleton :: a -> Vector a
singleton = error "todo"

replicate :: Int -> a -> Vector a
replicate = error "todo"

generate :: Int -> (Int -> a) -> Vector a
generate = error "todo"

iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN = error "todo"

replicateM :: (Monad m) => Int -> m a -> m (Vector a)
replicateM = error "todo"

generateM :: (Monad m) => Int -> (Int -> m a) -> m (Vector a)
generateM = error "todo"

iterateNM :: (Monad m) => Int -> (a -> m a) -> a -> m (Vector a)
iterateNM = error "todo"

unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr = error "todo"

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN = error "todo"

unfoldrExactN :: Int -> (b -> (a, b)) -> b -> Vector a
unfoldrExactN = error "todo"

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrM = error "todo"

unfoldrNM :: (Monad m) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrNM = error "todo"

unfoldrExactNM :: (Monad m) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
unfoldrExactNM = error "todo"

constructN :: Int -> (Vector a -> a) -> Vector a
constructN = error "todo"

constructrN :: Int -> (Vector a -> a) -> Vector a
constructrN = error "todo"

enumFromN :: (Num a) => a -> Int -> Vector a
enumFromN = error "todo"

enumFromStepN :: (Num a) => a -> a -> Int -> Vector a
enumFromStepN = error "todo"

enumFromTo :: (Enum a) => a -> a -> Vector a
enumFromTo = error "todo"

enumFromThenTo :: (Enum a) => a -> a -> a -> Vector a
enumFromThenTo = error "todo"

cons :: a -> Vector a -> Vector a
cons = error "todo"

snoc :: Vector a -> a -> Vector a
snoc = error "todo"

(++) :: Vector a -> Vector a -> Vector a
(++) = error "todo"

concat :: [Vector a] -> Vector a
concat = error "todo"

force :: Vector a -> Vector a
force = error "todo"

(//) :: Vector a -> [(Int, a)] -> Vector a
(//) = error "todo"

update :: Vector a -> Vector (Int, a) -> Vector a
update = error "todo"

update_ :: Vector a -> Vector Int -> Vector a -> Vector a
update_ = error "todo"

unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
unsafeUpd = error "todo"

unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
unsafeUpdate = error "todo"

unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
unsafeUpdate_ = error "todo"

accum :: (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
accum = error "todo"

accumulate :: (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
accumulate = error "todo"

accumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
accumulate_ = error "todo"

unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
unsafeAccum = error "todo"

unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
unsafeAccumulate = error "todo"

unsafeAccumulate_ :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
unsafeAccumulate_ = error "todo"

reverse :: Vector a -> Vector a
reverse = error "todo"

backpermute :: Vector a -> Vector Int -> Vector a
backpermute = error "todo"

unsafeBackpermute :: Vector a -> Vector Int -> Vector a
unsafeBackpermute = error "todo"

indexed :: Vector a -> Vector (Int, a)
indexed = error "todo"

map :: (a -> b) -> Vector a -> Vector b
map = error "todo"

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap = error "todo"

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap = error "todo"

mapM :: (Monad m) => (a -> m b) -> Vector a -> m (Vector b)
mapM = error "todo"

imapM :: (Monad m) => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM = error "todo"

mapM_ :: (Monad m) => (a -> m b) -> Vector a -> m ()
mapM_ = error "todo"

imapM_ :: (Monad m) => (Int -> a -> m b) -> Vector a -> m ()
imapM_ = error "todo"

forM :: (Monad m) => Vector a -> (a -> m b) -> m (Vector b)
forM = error "todo"

forM_ :: (Monad m) => Vector a -> (a -> m b) -> m ()
forM_ = error "todo"

iforM :: (Monad m) => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM = error "todo"

iforM_ :: (Monad m) => Vector a -> (Int -> a -> m b) -> m ()
iforM_ = error "todo"

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith = error "todo"

zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 = error "todo"

zipWith4 :: (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 = error "todo"

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
zipWith5 = error "todo"

zipWith6 ::
  (a -> b -> c -> d -> e -> f -> g) ->
  Vector a ->
  Vector b ->
  Vector c ->
  Vector d ->
  Vector e ->
  Vector f ->
  Vector g
zipWith6 = error "todo"

izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
izipWith = error "todo"

izipWith3 :: (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
izipWith3 = error "todo"

izipWith4 :: (Int -> a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
izipWith4 = error "todo"

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
izipWith5 = error "todo"

izipWith6 ::
  (Int -> a -> b -> c -> d -> e -> f -> g) ->
  Vector a ->
  Vector b ->
  Vector c ->
  Vector d ->
  Vector e ->
  Vector f ->
  Vector g
izipWith6 = error "todo"

zip :: Vector a -> Vector b -> Vector (a, b)
zip = error "todo"

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = error "todo"

zip4 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a, b, c, d)
zip4 = error "todo"

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a, b, c, d, e)
zip5 = error "todo"

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a, b, c, d, e, f)
zip6 = error "todo"

zipWithM :: (Monad m) => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
zipWithM = error "todo"

izipWithM :: (Monad m) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
izipWithM = error "todo"

zipWithM_ :: (Monad m) => (a -> b -> m c) -> Vector a -> Vector b -> m ()
zipWithM_ = error "todo"

izipWithM_ :: (Monad m) => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
izipWithM_ = error "todo"

unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip = error "todo"

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 = error "todo"

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 = error "todo"

unzip5 :: Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 = error "todo"

unzip6 :: Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
unzip6 = error "todo"

filter :: (a -> Bool) -> Vector a -> Vector a
filter = error "todo"

ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter = error "todo"

filterM :: (Monad m) => (a -> m Bool) -> Vector a -> m (Vector a)
filterM = error "todo"

uniq :: (Eq a) => Vector a -> Vector a
uniq = error "todo"

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe = error "todo"

imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe = error "todo"

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM = error "todo"

imapMaybeM :: (Monad m) => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM = error "todo"

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = error "todo"

takeWhile :: (a -> Bool) -> Vector a -> Vector a
takeWhile = error "todo"

dropWhile :: (a -> Bool) -> Vector a -> Vector a
dropWhile = error "todo"

partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition = error "todo"

unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
unstablePartition = error "todo"

partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
partitionWith = error "todo"

span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
span = error "todo"

break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
break = error "todo"

spanR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
spanR = error "todo"

breakR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
breakR = error "todo"

groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
groupBy = error "todo"

group :: (Eq a) => Vector a -> [Vector a]
group = error "todo"

elem :: (Eq a) => a -> Vector a -> Bool
elem = error "todo"

notElem :: (Eq a) => a -> Vector a -> Bool
notElem = error "todo"

find :: (a -> Bool) -> Vector a -> Maybe a
find = error "todo"

findIndex :: (a -> Bool) -> Vector a -> Maybe Int
findIndex = error "todo"

findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
findIndexR = error "todo"

findIndices :: (a -> Bool) -> Vector a -> Vector Int
findIndices = error "todo"

elemIndex :: (Eq a) => a -> Vector a -> Maybe Int
elemIndex = error "todo"

elemIndices :: (Eq a) => a -> Vector a -> Vector Int
elemIndices = error "todo"

foldl :: (a -> b -> a) -> a -> Vector b -> a
foldl = error "todo"

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 = error "todo"

foldl' :: (a -> b -> a) -> a -> Vector b -> a
foldl' = error "todo"

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' = error "todo"

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr = error "todo"

foldr1 :: (a -> a -> a) -> Vector a -> a
foldr1 = error "todo"

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' = error "todo"

foldr1' :: (a -> a -> a) -> Vector a -> a
foldr1' = error "todo"

ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl = error "todo"

ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl' = error "todo"

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr = error "todo"

ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' = error "todo"

foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap = error "todo"

foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap' = error "todo"

all :: (a -> Bool) -> Vector a -> Bool
all = error "todo"

any :: (a -> Bool) -> Vector a -> Bool
any = error "todo"

and :: Vector Bool -> Bool
and = error "todo"

or :: Vector Bool -> Bool
or = error "todo"

sum :: (Num a) => Vector a -> a
sum = error "todo"

product :: (Num a) => Vector a -> a
product = error "todo"

maximum :: (Ord a) => Vector a -> a
maximum = error "todo"

maximumBy :: (a -> a -> Ordering) -> Vector a -> a
maximumBy = error "todo"

maximumOn :: (Ord b) => (a -> b) -> Vector a -> a
maximumOn = error "todo"

minimum :: (Ord a) => Vector a -> a
minimum = error "todo"

minimumBy :: (a -> a -> Ordering) -> Vector a -> a
minimumBy = error "todo"

minimumOn :: (Ord b) => (a -> b) -> Vector a -> a
minimumOn = error "todo"

minIndex :: (Ord a) => Vector a -> Int
minIndex = error "todo"

minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
minIndexBy = error "todo"

maxIndex :: (Ord a) => Vector a -> Int
maxIndex = error "todo"

maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
maxIndexBy = error "todo"

foldM :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m a
foldM = error "todo"

ifoldM :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM = error "todo"

foldM' :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m a
foldM' = error "todo"

ifoldM' :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM' = error "todo"

fold1M :: (Monad m) => (a -> a -> m a) -> Vector a -> m a
fold1M = error "todo"

fold1M' :: (Monad m) => (a -> a -> m a) -> Vector a -> m a
fold1M' = error "todo"

foldM_ :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM_ = error "todo"

ifoldM_ :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM_ = error "todo"

foldM'_ :: (Monad m) => (a -> b -> m a) -> a -> Vector b -> m ()
foldM'_ = error "todo"

ifoldM'_ :: (Monad m) => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM'_ = error "todo"

fold1M_ :: (Monad m) => (a -> a -> m a) -> Vector a -> m ()
fold1M_ = error "todo"

fold1M'_ :: (Monad m) => (a -> a -> m a) -> Vector a -> m ()
fold1M'_ = error "todo"

sequence :: (Monad m) => Vector (m a) -> m (Vector a)
sequence = error "todo"

sequence_ :: (Monad m) => Vector (m a) -> m ()
sequence_ = error "todo"

prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl = error "todo"

prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl' = error "todo"

postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl = error "todo"

postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl' = error "todo"

scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl = error "todo"

scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl' = error "todo"

scanl1 :: (a -> a -> a) -> Vector a -> Vector a
scanl1 = error "todo"

scanl1' :: (a -> a -> a) -> Vector a -> Vector a
scanl1' = error "todo"

iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl = error "todo"

iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl' = error "todo"

prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr = error "todo"

prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr' = error "todo"

postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr = error "todo"

postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr' = error "todo"

scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr = error "todo"

scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr' = error "todo"

scanr1 :: (a -> a -> a) -> Vector a -> Vector a
scanr1 = error "todo"

scanr1' :: (a -> a -> a) -> Vector a -> Vector a
scanr1' = error "todo"

iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr = error "todo"

iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr' = error "todo"

eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
eqBy = error "todo"

cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy = error "todo"

toList :: Vector a -> [a]
toList = error "todo"

fromList :: [a] -> Vector a
fromList = error "todo"

fromListN :: Int -> [a] -> Vector a
fromListN = error "todo"

toLazy :: Vector a -> Lazy.Vector a
toLazy = error "todo"

fromLazy :: Lazy.Vector a -> Vector a
fromLazy = error "todo"
