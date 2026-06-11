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

import Hazy.Prelude (placeholder)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.Read.Lex (Lexeme (..))

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
