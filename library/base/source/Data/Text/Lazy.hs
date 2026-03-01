module Data.Text.Lazy
  ( Text,
    pack,
    unpack,
    singleton,
    empty,
    fromChunks,
    toChunks,
    toStrict,
    fromStrict,
    foldrChunks,
    foldlChunks,
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
    repeat,
    replicate,
    cycle,
    iterate,
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
    span,
    spanM,
    spanEndM,
    breakOn,
    breakOnEnd,
    break,
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
    find,
    elem,
    breakOnAll,
    partition,
    index,
    count,
    zip,
    zipWith,
  )
where

import Data.Int (Int64)
import Hazy (placeholder)

data Text

pack :: String -> Text
pack = placeholder

unpack :: Text -> String
unpack = placeholder

singleton :: Char -> Text
singleton = placeholder

empty :: Text
empty = placeholder

fromChunks :: [Text] -> Text
fromChunks = placeholder

toChunks :: Text -> [Text]
toChunks = placeholder

toStrict :: Text -> Text
toStrict = placeholder

fromStrict :: Text -> Text
fromStrict = placeholder

foldrChunks :: (Text -> a -> a) -> a -> Text -> a
foldrChunks = placeholder

foldlChunks :: (a -> Text -> a) -> a -> Text -> a
foldlChunks = placeholder

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

length :: Text -> Int64
length = placeholder

compareLength :: Text -> Int64 -> Ordering
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

justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft = placeholder

justifyRight :: Int64 -> Char -> Text -> Text
justifyRight = placeholder

center :: Int64 -> Char -> Text -> Text
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

repeat :: Char -> Text
repeat = placeholder

replicate :: Int64 -> Text -> Text
replicate = placeholder

cycle :: Text -> Text
cycle = placeholder

iterate :: (Char -> Char) -> Char -> Text
iterate = placeholder

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr = placeholder

unfoldrN :: Int64 -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN = placeholder

take :: Int64 -> Text -> Text
take = placeholder

takeEnd :: Int64 -> Text -> Text
takeEnd = placeholder

drop :: Int64 -> Text -> Text
drop = placeholder

dropEnd :: Int64 -> Text -> Text
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

splitAt :: Int64 -> Text -> (Text, Text)
splitAt = placeholder

span :: (Char -> Bool) -> Text -> (Text, Text)
span = placeholder

spanM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanM = placeholder

spanEndM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM = placeholder

breakOn :: Text -> Text -> (Text, Text)
breakOn = placeholder

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd = placeholder

break :: (Char -> Bool) -> Text -> (Text, Text)
break = placeholder

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

chunksOf :: Int64 -> Text -> [Text]
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

find :: (Char -> Bool) -> Text -> Maybe Char
find = placeholder

elem :: Char -> Text -> Bool
elem = placeholder

breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll = placeholder

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition = placeholder

index :: Text -> Int64 -> Char
index = placeholder

count :: Text -> Text -> Int64
count = placeholder

zip :: Text -> Text -> [(Char, Char)]
zip = placeholder

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith = placeholder
