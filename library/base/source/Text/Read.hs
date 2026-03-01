module Text.Read
  ( Read (..),
    ReadS,
    reads,
    read,
    readParen,
    lex,
    module Text.ParserCombinators.ReadPrec,
    Lexeme (..),
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
    readEither,
    readMaybe,
  )
where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Either (Either)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String)
import Hazy (placeholder)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.Read.Lex (Lexeme (..))
import Prelude (error)

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  readPrec :: ReadPrec a
  readListPrec :: ReadPrec [a]

type ReadS a = String -> [(a, String)]

reads :: (Read a) => ReadS a
reads = placeholder

read :: (Read a) => String -> a
read = placeholder

readParen :: Bool -> ReadS a -> ReadS a
readParen = placeholder

lex :: ReadS String
lex = placeholder

lexP :: ReadPrec Lexeme
lexP = placeholder

parens :: ReadPrec a -> ReadPrec a
parens = placeholder

readListDefault :: (Read a) => ReadS [a]
readListDefault = placeholder

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = placeholder

readEither :: (Read a) => String -> Either String a
readEither = placeholder

readMaybe :: (Read a) => String -> Maybe a
readMaybe = placeholder
