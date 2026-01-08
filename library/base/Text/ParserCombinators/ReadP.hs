{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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
import Text.Read (ReadS)
import Prelude (error)

data ReadP a

get :: ReadP Char
get = error "todo"

look :: ReadP String
look = error "todo"

infixr 5 +++

(+++) :: ReadP a -> ReadP a -> ReadP a
(+++) = error "todo"

infixr 5 <++

(<++) :: ReadP a -> ReadP a -> ReadP a
(<++) = error "todo"

gather :: ReadP a -> ReadP (String, a)
gather = error "todo"

pfail :: ReadP a
pfail = error "todo"

eof :: ReadP ()
eof = error "todo"

satisfy :: (Char -> Bool) -> ReadP Char
satisfy = error "todo"

char :: Char -> ReadP Char
char = error "todo"

string :: String -> ReadP String
string = error "todo"

munch :: (Char -> Bool) -> ReadP String
munch = error "todo"

munch1 :: (Char -> Bool) -> ReadP String
munch1 = error "todo"

skipSpaces :: ReadP ()
skipSpaces = error "todo"

choice :: [ReadP a] -> ReadP a
choice = error "todo"

count :: Int -> ReadP a -> ReadP [a]
count = error "todo"

between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
between = error "todo"

option :: a -> ReadP a -> ReadP a
option = error "todo"

optional :: ReadP a -> ReadP ()
optional = error "todo"

many :: ReadP a -> ReadP [a]
many = error "todo"

many1 :: ReadP a -> ReadP [a]
many1 = error "todo"

skipMany :: ReadP a -> ReadP ()
skipMany = error "todo"

skipMany1 :: ReadP a -> ReadP ()
skipMany1 = error "todo"

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy = error "todo"

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 = error "todo"

endBy :: ReadP a -> ReadP sep -> ReadP [a]
endBy = error "todo"

endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
endBy1 = error "todo"

chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainr = error "todo"

chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainl = error "todo"

chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 = error "todo"

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 = error "todo"

manyTill :: ReadP a -> ReadP end -> ReadP [a]
manyTill = error "todo"

readP_to_S :: ReadP a -> ReadS a
readP_to_S = error "todo"

readS_to_P :: ReadS a -> ReadP a
readS_to_P = error "todo"
