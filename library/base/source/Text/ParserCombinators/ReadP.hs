module Text.ParserCombinators.ReadP
  ( ReadP,
    get,
    look,
    (+++),
    (<++),
    gather,
    pfail,
    eof,
    satisfy,
    char,
    string,
    munch,
    munch1,
    skipSpaces,
    choice,
    count,
    between,
    option,
    optional,
    many,
    many1,
    skipMany,
    skipMany1,
    sepBy,
    sepBy1,
    endBy,
    endBy1,
    chainr,
    chainl,
    chainl1,
    chainr1,
    manyTill,
    ReadS,
    readP_to_S,
    readS_to_P,
  )
where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Int (Int)
import Data.String (String)
import Hazy.Prelude (placeholder)
import Text.Read (ReadS)
import Prelude (error)

data ReadP a

get :: ReadP Char
get = placeholder

look :: ReadP String
look = placeholder

infixr 5 +++

(+++) :: ReadP a -> ReadP a -> ReadP a
(+++) = placeholder

infixr 5 <++

(<++) :: ReadP a -> ReadP a -> ReadP a
(<++) = placeholder

gather :: ReadP a -> ReadP (String, a)
gather = placeholder

pfail :: ReadP a
pfail = placeholder

eof :: ReadP ()
eof = placeholder

satisfy :: (Char -> Bool) -> ReadP Char
satisfy = placeholder

char :: Char -> ReadP Char
char = placeholder

string :: String -> ReadP String
string = placeholder

munch :: (Char -> Bool) -> ReadP String
munch = placeholder

munch1 :: (Char -> Bool) -> ReadP String
munch1 = placeholder

skipSpaces :: ReadP ()
skipSpaces = placeholder

choice :: [ReadP a] -> ReadP a
choice = placeholder

count :: Int -> ReadP a -> ReadP [a]
count = placeholder

between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
between = placeholder

option :: a -> ReadP a -> ReadP a
option = placeholder

optional :: ReadP a -> ReadP ()
optional = placeholder

many :: ReadP a -> ReadP [a]
many = placeholder

many1 :: ReadP a -> ReadP [a]
many1 = placeholder

skipMany :: ReadP a -> ReadP ()
skipMany = placeholder

skipMany1 :: ReadP a -> ReadP ()
skipMany1 = placeholder

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy = placeholder

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 = placeholder

endBy :: ReadP a -> ReadP sep -> ReadP [a]
endBy = placeholder

endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
endBy1 = placeholder

chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainr = placeholder

chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainl = placeholder

chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 = placeholder

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 = placeholder

manyTill :: ReadP a -> ReadP end -> ReadP [a]
manyTill = placeholder

readP_to_S :: ReadP a -> ReadS a
readP_to_S = placeholder

readS_to_P :: ReadS a -> ReadP a
readS_to_P = placeholder
