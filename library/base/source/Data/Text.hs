module Data.Text
  ( Text,
    pack,
    unpack,
    singleton,
    empty,
    cons,
    snoc,
    append,
    uncons,
    unsnoc,
    head,
    last,
    tail,
    init,
    null,
    length,
    compareLength,
    map,
    intercalate,
    intersperse,
    transpose,
    reverse,
    replace,
    toCaseFold,
    toLower,
    toUpper,
    toTitle,
    justifyLeft,
    justifyRight,
    center,
    foldl,
    foldl,
    foldl1,
    foldl1,
    foldr,
    foldr,
    foldr1,
    concat,
    concatMap,
    any,
    all,
    maximum,
    minimum,
    isAscii,
    scanl,
    scanl1,
    scanr,
    scanr1,
    mapAccumL,
    mapAccumR,
    replicate,
    unfoldr,
    unfoldrN,
    take,
    takeEnd,
    drop,
    dropEnd,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    dropAround,
    strip,
    stripStart,
    stripEnd,
    splitAt,
    breakOn,
    breakOnEnd,
    break,
    span,
    spanM,
    spanEndM,
    group,
    groupBy,
    inits,
    tails,
    splitOn,
    split,
    chunksOf,
    lines,
    words,
    unlines,
    unwords,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
    stripPrefix,
    stripSuffix,
    commonPrefixes,
    filter,
    breakOnAll,
    find,
    elem,
    partition,
    index,
    findIndex,
    count,
    zip,
    zipWith,
    copy,
    measureOff,
  )
where

import Hazy.Prelude (Text, pack, placeholder)

unpack :: Text -> String
unpack = placeholder

singleton :: Char -> Text
singleton = placeholder

empty :: Text
empty = placeholder

infixr 5 `cons`

cons :: Char -> Text -> Text
cons = placeholder

snoc :: Text -> Char -> Text
snoc = placeholder

append :: Text -> Text -> Text
append = placeholder

uncons :: Text -> Maybe (Char, Text)
uncons = placeholder

unsnoc :: Text -> Maybe (Text, Char)
unsnoc = placeholder

head :: Text -> Char
head = placeholder

last :: Text -> Char
last = placeholder

tail :: Text -> Text
tail = placeholder

init :: Text -> Text
init = placeholder

null :: Text -> Bool
null = placeholder

length :: Text -> Int
length = placeholder

compareLength :: Text -> Int -> Ordering
compareLength = placeholder

map :: (Char -> Char) -> Text -> Text
map = placeholder

intercalate :: Text -> [Text] -> Text
intercalate = placeholder

intersperse :: Char -> Text -> Text
intersperse = placeholder

transpose :: [Text] -> [Text]
transpose = placeholder

reverse :: Text -> Text
reverse = placeholder

replace :: Text -> Text -> Text -> Text
replace = placeholder

toCaseFold :: Text -> Text
toCaseFold = placeholder

toLower :: Text -> Text
toLower = placeholder

toUpper :: Text -> Text
toUpper = placeholder

toTitle :: Text -> Text
toTitle = placeholder

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft = placeholder

justifyRight :: Int -> Char -> Text -> Text
justifyRight = placeholder

center :: Int -> Char -> Text -> Text
center = placeholder

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl = placeholder

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' = placeholder

foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 = placeholder

foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' = placeholder

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr = placeholder

foldr' :: (Char -> a -> a) -> a -> Text -> a
foldr' = placeholder

foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 = placeholder

concat :: [Text] -> Text
concat = placeholder

concatMap :: (Char -> Text) -> Text -> Text
concatMap = placeholder

any :: (Char -> Bool) -> Text -> Bool
any = placeholder

all :: (Char -> Bool) -> Text -> Bool
all = placeholder

maximum :: Text -> Char
maximum = placeholder

minimum :: Text -> Char
minimum = placeholder

isAscii :: Text -> Bool
isAscii = placeholder

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl = placeholder

scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 = placeholder

scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr = placeholder

scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 = placeholder

mapAccumL :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumL = placeholder

mapAccumR :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumR = placeholder

replicate :: Int -> Text -> Text
replicate = placeholder

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr = placeholder

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN = placeholder

take :: Int -> Text -> Text
take = placeholder

takeEnd :: Int -> Text -> Text
takeEnd = placeholder

drop :: Int -> Text -> Text
drop = placeholder

dropEnd :: Int -> Text -> Text
dropEnd = placeholder

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile = placeholder

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd = placeholder

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile = placeholder

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd = placeholder

dropAround :: (Char -> Bool) -> Text -> Text
dropAround = placeholder

strip :: Text -> Text
strip = placeholder

stripStart :: Text -> Text
stripStart = placeholder

stripEnd :: Text -> Text
stripEnd = placeholder

splitAt :: Int -> Text -> (Text, Text)
splitAt = placeholder

breakOn :: Text -> Text -> (Text, Text)
breakOn = placeholder

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd = placeholder

break :: (Char -> Bool) -> Text -> (Text, Text)
break = placeholder

span :: (Char -> Bool) -> Text -> (Text, Text)
span = placeholder

spanM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanM = placeholder

spanEndM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM = placeholder

group :: Text -> [Text]
group = placeholder

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy = placeholder

inits :: Text -> [Text]
inits = placeholder

tails :: Text -> [Text]
tails = placeholder

splitOn :: Text -> Text -> [Text]
splitOn = placeholder

split :: (Char -> Bool) -> Text -> [Text]
split = placeholder

chunksOf :: Int -> Text -> [Text]
chunksOf = placeholder

lines :: Text -> [Text]
lines = placeholder

words :: Text -> [Text]
words = placeholder

unlines :: [Text] -> Text
unlines = placeholder

unwords :: [Text] -> Text
unwords = placeholder

isPrefixOf :: Text -> Text -> Bool
isPrefixOf = placeholder

isSuffixOf :: Text -> Text -> Bool
isSuffixOf = placeholder

isInfixOf :: Text -> Text -> Bool
isInfixOf = placeholder

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix = placeholder

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix = placeholder

commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes = placeholder

filter :: (Char -> Bool) -> Text -> Text
filter = placeholder

breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll = placeholder

find :: (Char -> Bool) -> Text -> Maybe Char
find = placeholder

elem :: Char -> Text -> Bool
elem = placeholder

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition = placeholder

index :: Text -> Int -> Char
index = placeholder

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex = placeholder

count :: Text -> Text -> Int
count = placeholder

zip :: Text -> Text -> [(Char, Char)]
zip = placeholder

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith = placeholder

copy :: Text -> Text
copy = placeholder

measureOff :: Int -> Text -> Int
measureOff = placeholder
