{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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

data Text

pack :: String -> Text
pack = error "todo"

unpack :: Text -> String
unpack = error "todo"

singleton :: Char -> Text
singleton = error "todo"

empty :: Text
empty = error "todo"

fromChunks :: [Text] -> Text
fromChunks = error "todo"

toChunks :: Text -> [Text]
toChunks = error "todo"

toStrict :: Text -> Text
toStrict = error "todo"

fromStrict :: Text -> Text
fromStrict = error "todo"

foldrChunks :: (Text -> a -> a) -> a -> Text -> a
foldrChunks = error "todo"

foldlChunks :: (a -> Text -> a) -> a -> Text -> a
foldlChunks = error "todo"

infixr 5 `cons`

cons :: Char -> Text -> Text
cons = error "todo"

snoc :: Text -> Char -> Text
snoc = error "todo"

append :: Text -> Text -> Text
append = error "todo"

uncons :: Text -> Maybe (Char, Text)
uncons = error "todo"

unsnoc :: Text -> Maybe (Text, Char)
unsnoc = error "todo"

head :: Text -> Char
head = error "todo"

last :: Text -> Char
last = error "todo"

tail :: Text -> Text
tail = error "todo"

init :: Text -> Text
init = error "todo"

null :: Text -> Bool
null = error "todo"

length :: Text -> Int64
length = error "todo"

compareLength :: Text -> Int64 -> Ordering
compareLength = error "todo"

map :: (Char -> Char) -> Text -> Text
map = error "todo"

intercalate :: Text -> [Text] -> Text
intercalate = error "todo"

intersperse :: Char -> Text -> Text
intersperse = error "todo"

transpose :: [Text] -> [Text]
transpose = error "todo"

reverse :: Text -> Text
reverse = error "todo"

replace :: Text -> Text -> Text -> Text
replace = error "todo"

toCaseFold :: Text -> Text
toCaseFold = error "todo"

toLower :: Text -> Text
toLower = error "todo"

toUpper :: Text -> Text
toUpper = error "todo"

toTitle :: Text -> Text
toTitle = error "todo"

justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft = error "todo"

justifyRight :: Int64 -> Char -> Text -> Text
justifyRight = error "todo"

center :: Int64 -> Char -> Text -> Text
center = error "todo"

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl = error "todo"

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' = error "todo"

foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 = error "todo"

foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' = error "todo"

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr = error "todo"

foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 = error "todo"

concat :: [Text] -> Text
concat = error "todo"

concatMap :: (Char -> Text) -> Text -> Text
concatMap = error "todo"

any :: (Char -> Bool) -> Text -> Bool
any = error "todo"

all :: (Char -> Bool) -> Text -> Bool
all = error "todo"

maximum :: Text -> Char
maximum = error "todo"

minimum :: Text -> Char
minimum = error "todo"

isAscii :: Text -> Bool
isAscii = error "todo"

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl = error "todo"

scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 = error "todo"

scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr = error "todo"

scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 = error "todo"

mapAccumL :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumL = error "todo"

mapAccumR :: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumR = error "todo"

repeat :: Char -> Text
repeat = error "todo"

replicate :: Int64 -> Text -> Text
replicate = error "todo"

cycle :: Text -> Text
cycle = error "todo"

iterate :: (Char -> Char) -> Char -> Text
iterate = error "todo"

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr = error "todo"

unfoldrN :: Int64 -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN = error "todo"

take :: Int64 -> Text -> Text
take = error "todo"

takeEnd :: Int64 -> Text -> Text
takeEnd = error "todo"

drop :: Int64 -> Text -> Text
drop = error "todo"

dropEnd :: Int64 -> Text -> Text
dropEnd = error "todo"

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile = error "todo"

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd = error "todo"

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile = error "todo"

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd = error "todo"

dropAround :: (Char -> Bool) -> Text -> Text
dropAround = error "todo"

strip :: Text -> Text
strip = error "todo"

stripStart :: Text -> Text
stripStart = error "todo"

stripEnd :: Text -> Text
stripEnd = error "todo"

splitAt :: Int64 -> Text -> (Text, Text)
splitAt = error "todo"

span :: (Char -> Bool) -> Text -> (Text, Text)
span = error "todo"

spanM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanM = error "todo"

spanEndM :: (Monad m) => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM = error "todo"

breakOn :: Text -> Text -> (Text, Text)
breakOn = error "todo"

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd = error "todo"

break :: (Char -> Bool) -> Text -> (Text, Text)
break = error "todo"

group :: Text -> [Text]
group = error "todo"

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy = error "todo"

inits :: Text -> [Text]
inits = error "todo"

tails :: Text -> [Text]
tails = error "todo"

splitOn :: Text -> Text -> [Text]
splitOn = error "todo"

split :: (Char -> Bool) -> Text -> [Text]
split = error "todo"

chunksOf :: Int64 -> Text -> [Text]
chunksOf = error "todo"

lines :: Text -> [Text]
lines = error "todo"

words :: Text -> [Text]
words = error "todo"

unlines :: [Text] -> Text
unlines = error "todo"

unwords :: [Text] -> Text
unwords = error "todo"

isPrefixOf :: Text -> Text -> Bool
isPrefixOf = error "todo"

isSuffixOf :: Text -> Text -> Bool
isSuffixOf = error "todo"

isInfixOf :: Text -> Text -> Bool
isInfixOf = error "todo"

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix = error "todo"

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix = error "todo"

commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes = error "todo"

filter :: (Char -> Bool) -> Text -> Text
filter = error "todo"

find :: (Char -> Bool) -> Text -> Maybe Char
find = error "todo"

elem :: Char -> Text -> Bool
elem = error "todo"

breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll = error "todo"

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition = error "todo"

index :: Text -> Int64 -> Char
index = error "todo"

count :: Text -> Text -> Int64
count = error "todo"

zip :: Text -> Text -> [(Char, Char)]
zip = error "todo"

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith = error "todo"
