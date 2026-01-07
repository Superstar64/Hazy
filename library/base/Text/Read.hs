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
reads = error "todo"

read :: (Read a) => String -> a
read = error "todo"

readParen :: Bool -> ReadS a -> ReadS a
readParen = error "todo"

lex :: ReadS String
lex = error "todo"

lexP :: ReadPrec Lexeme
lexP = error "todo"

parens :: ReadPrec a -> ReadPrec a
parens = error "todo"

readListDefault :: (Read a) => ReadS [a]
readListDefault = error "todo"

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = error "todo"

readEither :: (Read a) => String -> Either String a
readEither = error "todo"

readMaybe :: (Read a) => String -> Maybe a
readMaybe = error "todo"
