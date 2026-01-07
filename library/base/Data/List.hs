module Data.List
  ( (++),
    head,
    last,
    tail,
    init,
    uncons,
    singleton,
    null,
    length,
    map,
    reverse,
    intersperse,
    intercalate,
    transpose,
    subsequences,
    permutations,
    foldl,
    foldl,
    foldl1,
    foldl1,
    foldr,
    foldr1,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    sum,
    product,
    maximum,
    minimum,
    scanl,
    scanl,
    scanl1,
    scanr,
    scanr1,
    mapAccumL,
    mapAccumR,
    iterate,
    iterate,
    repeat,
    replicate,
    cycle,
    unfoldr,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    dropWhileEnd,
    span,
    break,
    stripPrefix,
    group,
    inits,
    tails,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
    isSubsequenceOf,
    elem,
    notElem,
    lookup,
    find,
    filter,
    partition,
    (!!),
    elemIndex,
    elemIndices,
    findIndex,
    findIndices,
    zip,
    zip3,
    zip4,
    zip5,
    zip6,
    zip7,
    zipWith,
    zipWith3,
    zipWith4,
    zipWith5,
    zipWith6,
    zipWith7,
    unzip,
    unzip3,
    unzip4,
    unzip5,
    unzip6,
    unzip7,
    lines,
    words,
    unlines,
    unwords,
    nub,
    delete,
    (\\),
    union,
    intersect,
    sort,
    sortOn,
    insert,
    nubBy,
    deleteBy,
    deleteFirstsBy,
    unionBy,
    intersectBy,
    groupBy,
    sortBy,
    insertBy,
    maximumBy,
    minimumBy,
    genericLength,
    genericTake,
    genericDrop,
    genericSplitAt,
    genericIndex,
    genericReplicate,
  )
where

import Data.Bool (Bool)
import Data.Eq
  ( Eq,
  )
import Data.Foldable
  ( all,
    and,
    any,
    concat,
    concatMap,
    elem,
    find,
    foldl,
    foldl',
    foldl1,
    foldl1',
    foldr,
    foldr1,
    length,
    maximum,
    maximumBy,
    minimum,
    minimumBy,
    notElem,
    null,
    or,
    product,
    sum,
  )
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Ord
  ( Ord,
    Ordering,
  )
import Data.String
  ( String,
    lines,
    unlines,
    unwords,
    words,
  )
import Data.Traversable
  ( mapAccumL,
    mapAccumR,
  )
import Prelude (Integral, Num, error)

infixr 5 ++

(++) :: [a] -> [a] -> [a]
(++) = error "todo"

head :: [a] -> a
head = error "todo"

last :: [a] -> a
last = error "todo"

tail :: [a] -> [a]
tail = error "todo"

init :: [a] -> [a]
init = error "todo"

uncons :: [a] -> Maybe (a, [a])
uncons = error "todo"

singleton :: a -> [a]
singleton = error "todo"

map :: (a -> b) -> [a] -> [b]
map = error "todo"

reverse :: [a] -> [a]
reverse = error "todo"

intersperse :: a -> [a] -> [a]
intersperse = error "todo"

intercalate :: [a] -> [[a]] -> [a]
intercalate = error "todo"

transpose :: [[a]] -> [[a]]
transpose = error "todo"

subsequences :: [a] -> [[a]]
subsequences = error "todo"

permutations :: [a] -> [[a]]
permutations = error "todo"

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = error "todo"

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' = error "todo"

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 = error "todo"

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = error "todo"

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 = error "todo"

iterate :: (a -> a) -> a -> [a]
iterate = error "todo"

iterate' :: (a -> a) -> a -> [a]
iterate' = error "todo"

repeat :: a -> [a]
repeat = error "todo"

replicate :: Int -> a -> [a]
replicate = error "todo"

cycle :: [a] -> [a]
cycle = error "todo"

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr = error "todo"

take :: Int -> [a] -> [a]
take = error "todo"

drop :: Int -> [a] -> [a]
drop = error "todo"

splitAt :: Int -> [a] -> ([a], [a])
splitAt = error "todo"

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = error "todo"

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = error "todo"

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd = error "todo"

span :: (a -> Bool) -> [a] -> ([a], [a])
span = error "todo"

break :: (a -> Bool) -> [a] -> ([a], [a])
break = error "todo"

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix = error "todo"

group :: (Eq a) => [a] -> [[a]]
group = error "todo"

inits :: [a] -> [[a]]
inits = error "todo"

tails :: [a] -> [[a]]
tails = error "todo"

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf = error "todo"

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf = error "todo"

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf = error "todo"

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf = error "todo"

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup = error "todo"

filter :: (a -> Bool) -> [a] -> [a]
filter = error "todo"

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = error "todo"

(!!) :: [a] -> Int -> a
(!!) = error "todo"

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex = error "todo"

elemIndices :: (Eq a) => a -> [a] -> [Int]
elemIndices = error "todo"

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex = error "todo"

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices = error "todo"

zip :: [a] -> [b] -> [(a, b)]
zip = error "todo"

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = error "todo"

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = error "todo"

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = error "todo"

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = error "todo"

zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zip7 = error "todo"

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = error "todo"

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 = error "todo"

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 = error "todo"

zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 = error "todo"

zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 = error "todo"

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 = error "todo"

unzip :: [(a, b)] -> ([a], [b])
unzip = error "todo"

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = error "todo"

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = error "todo"

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 = error "todo"

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 = error "todo"

unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 = error "todo"

nub :: (Eq a) => [a] -> [a]
nub = error "todo"

delete :: (Eq a) => a -> [a] -> [a]
delete = error "todo"

infix 5 \\

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = error "todo"

union :: (Eq a) => [a] -> [a] -> [a]
union = error "todo"

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect = error "todo"

sort :: (Ord a) => [a] -> [a]
sort = error "todo"

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn = error "todo"

insert :: (Ord a) => a -> [a] -> [a]
insert = error "todo"

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy = error "todo"

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy = error "todo"

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy = error "todo"

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy = error "todo"

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy = error "todo"

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = error "todo"

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = error "todo"

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy = error "todo"

genericLength :: (Num i) => [a] -> i
genericLength = error "todo"

genericTake :: (Integral i) => i -> [a] -> [a]
genericTake = error "todo"

genericDrop :: (Integral i) => i -> [a] -> [a]
genericDrop = error "todo"

genericSplitAt :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt = error "todo"

genericIndex :: (Integral i) => [a] -> i -> a
genericIndex = error "todo"

genericReplicate :: (Integral i) => i -> a -> [a]
genericReplicate = error "todo"
