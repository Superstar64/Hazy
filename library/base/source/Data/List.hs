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
    iterate',
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
import Hazy.Prelude
  ( break,
    cycle,
    drop,
    dropWhile,
    filter,
    head,
    init,
    iterate,
    last,
    lookup,
    map,
    placeholder,
    repeat,
    replicate,
    reverse,
    span,
    splitAt,
    tail,
    take,
    takeWhile,
    unzip,
    unzip3,
    zip,
    zip3,
    zipWith,
    zipWith3,
    (!!),
    (++),
  )
import Prelude (Integral, Num, error)

uncons :: [a] -> Maybe (a, [a])
uncons = placeholder

singleton :: a -> [a]
singleton = placeholder

intersperse :: a -> [a] -> [a]
intersperse = placeholder

intercalate :: [a] -> [[a]] -> [a]
intercalate = placeholder

transpose :: [[a]] -> [[a]]
transpose = placeholder

subsequences :: [a] -> [[a]]
subsequences = placeholder

permutations :: [a] -> [[a]]
permutations = placeholder

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = placeholder

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' = placeholder

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 = placeholder

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = placeholder

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 = placeholder

iterate' :: (a -> a) -> a -> [a]
iterate' = placeholder

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr = placeholder

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd = placeholder

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix = placeholder

group :: (Eq a) => [a] -> [[a]]
group = placeholder

inits :: [a] -> [[a]]
inits = placeholder

tails :: [a] -> [[a]]
tails = placeholder

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf = placeholder

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf = placeholder

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf = placeholder

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf = placeholder

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = placeholder

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex = placeholder

elemIndices :: (Eq a) => a -> [a] -> [Int]
elemIndices = placeholder

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex = placeholder

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices = placeholder

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = placeholder

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = placeholder

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = placeholder

zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zip7 = placeholder

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 = placeholder

zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 = placeholder

zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 = placeholder

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 = placeholder

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = placeholder

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 = placeholder

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 = placeholder

unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 = placeholder

nub :: (Eq a) => [a] -> [a]
nub = placeholder

delete :: (Eq a) => a -> [a] -> [a]
delete = placeholder

infix 5 \\

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = placeholder

union :: (Eq a) => [a] -> [a] -> [a]
union = placeholder

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect = placeholder

sort :: (Ord a) => [a] -> [a]
sort = placeholder

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn = placeholder

insert :: (Ord a) => a -> [a] -> [a]
insert = placeholder

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy = placeholder

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy = placeholder

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy = placeholder

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy = placeholder

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy = placeholder

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = placeholder

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = placeholder

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy = placeholder

genericLength :: (Num i) => [a] -> i
genericLength = placeholder

genericTake :: (Integral i) => i -> [a] -> [a]
genericTake = placeholder

genericDrop :: (Integral i) => i -> [a] -> [a]
genericDrop = placeholder

genericSplitAt :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt = placeholder

genericIndex :: (Integral i) => [a] -> i -> a
genericIndex = placeholder

genericReplicate :: (Integral i) => i -> a -> [a]
genericReplicate = placeholder
