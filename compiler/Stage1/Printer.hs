module Stage1.Printer (Printed, token, token', build) where

import Data.Text.Lazy.Builder (Builder, fromString)
import Stage1.Lexer (Lexeme, formatLexeme, lex)
import Prelude hiding (lex)

newtype Printed = Printed ([Lexeme] -> [Lexeme])

instance Semigroup Printed where
  Printed left <> Printed right = Printed (left . right)

instance Monoid Printed where
  mempty = Printed id

token :: String -> Printed
token = token' . lex

token' :: Lexeme -> Printed
token' lexeme = Printed (lexeme :)

build :: Printed -> Builder
build (Printed tokens) = go (tokens [])
  where
    go [] = mempty
    go [token] = formatLexeme token
    go (token : tokens) = formatLexeme token <> fromString " " <> go tokens
