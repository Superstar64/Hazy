module Data.List.NonEmpty
  ( NonEmpty (..),
    map,
    intersperse,
    scanl,
    scanr,
    scanl1,
    scanr1,
    transpose,
    sortBy,
    sortWith,
    length,
    head,
    tail,
    last,
    init,
    singleton,
    (<|),
    cons,
    uncons,
    unfoldr,
    sort,
    reverse,
    inits,
    tails,
    append,
    appendList,
    prependList,
    iterate,
    repeat,
    cycle,
    unfold,
    insert,
    some1,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    filter,
    partition,
    group,
    groupBy,
    groupWith,
    groupAllWith,
    group1,
    groupBy1,
    groupWith1,
    groupAllWith1,
    isPrefixOf,
    nub,
    nubBy,
    (!!),
    zip,
    zipWith,
    unzip,
    fromList,
    toList,
    nonEmpty,
    xor,
  )
where

import Control.Applicative (Alternative (..))

infixr 5 :|

data NonEmpty a = a :| [a]

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map = error "todo"

intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse = error "todo"

scanl :: (Foldable f) => (b -> a -> b) -> b -> f a -> NonEmpty b
scanl = error "todo"

scanr :: (Foldable f) => (a -> b -> b) -> b -> f a -> NonEmpty b
scanr = error "todo"

scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 = error "todo"

scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 = error "todo"

transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = error "todo"

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy = error "todo"

sortWith :: (Ord o) => (a -> o) -> NonEmpty a -> NonEmpty a
sortWith = error "todo"

length :: NonEmpty a -> Int
length = error "todo"

head :: NonEmpty a -> a
head = error "todo"

tail :: NonEmpty a -> [a]
tail = error "todo"

last :: NonEmpty a -> a
last = error "todo"

init :: NonEmpty a -> [a]
init = error "todo"

singleton :: a -> NonEmpty a
singleton = error "todo"

infixr 5 <|

(<|) :: a -> NonEmpty a -> NonEmpty a
(<|) = error "todo"

cons :: a -> NonEmpty a -> NonEmpty a
cons = error "todo"

uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons = error "todo"

unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr = error "todo"

sort :: (Ord a) => NonEmpty a -> NonEmpty a
sort = error "todo"

reverse :: NonEmpty a -> NonEmpty a
reverse = error "todo"

inits :: (Foldable f) => f a -> NonEmpty [a]
inits = error "todo"

tails :: (Foldable f) => f a -> NonEmpty [a]
tails = error "todo"

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append = error "todo"

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList = error "todo"

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList = error "todo"

iterate :: (a -> a) -> a -> NonEmpty a
iterate = error "todo"

repeat :: a -> NonEmpty a
repeat = error "todo"

cycle :: NonEmpty a -> NonEmpty a
cycle = error "todo"

unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfold = error "todo"

insert :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert = error "todo"

some1 :: (Alternative f) => f a -> f (NonEmpty a)
some1 = error "todo"

take :: Int -> NonEmpty a -> [a]
take = error "todo"

drop :: Int -> NonEmpty a -> [a]
drop = error "todo"

splitAt :: Int -> NonEmpty a -> ([a], [a])
splitAt = error "todo"

takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile = error "todo"

dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile = error "todo"

span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span = error "todo"

break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break = error "todo"

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter = error "todo"

partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition = error "todo"

group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = error "todo"

groupBy :: (Foldable f) => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy = error "todo"

groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith = error "todo"

groupAllWith :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith = error "todo"

group1 :: (Eq a) => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = error "todo"

groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 = error "todo"

groupWith1 :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 = error "todo"

groupAllWith1 :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 = error "todo"

isPrefixOf :: (Eq a) => [a] -> NonEmpty a -> Bool
isPrefixOf = error "todo"

nub :: (Eq a) => NonEmpty a -> NonEmpty a
nub = error "todo"

nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
nubBy = error "todo"

(!!) :: NonEmpty a -> Int -> a
(!!) = error "todo"

zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip = error "todo"

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith = error "todo"

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip = error "todo"

fromList :: [a] -> NonEmpty a
fromList = error "todo"

toList :: NonEmpty a -> [a]
toList = error "todo"

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty = error "todo"

xor :: NonEmpty Bool -> Bool
xor = error "todo"
