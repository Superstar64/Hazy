module Javascript.Printer.Lexer
  ( Lexer,
    Print (..),
    Identifier,
    identifier,
    String,
    string,
    Number,
    int,
    bigInt,
    token,
    run,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import Prelude hiding (String, print)
import qualified Prelude

newtype Lexer = Lexer Builder

instance Semigroup Lexer where
  Lexer left <> Lexer right = Lexer (left <> right)

instance Monoid Lexer where
  mempty = Lexer mempty

class Print ast where
  print :: ast -> Lexer

instance (Print ast) => Print (Maybe ast) where
  print = foldMap print

instance (Print ast) => Print [ast] where
  print = foldMap print

instance (Print ast) => Print (NonEmpty ast) where
  print = foldMap print

newtype Identifier = Identifier Lexer

instance Print Identifier where
  print (Identifier ast) = ast

identifier :: Text -> Identifier
identifier name = Identifier $ Lexer $ fromText name <> fromString " "

newtype String = String Lexer

instance Print String where
  print (String ast) = ast

string :: Text -> String
string text = String $ Lexer $ fromString "\"" <> fromText text <> fromString "\"" <> fromString " "

newtype Number = Number Lexer

instance Print Number where
  print (Number ast) = ast

int :: Int -> Number
int number = Number $ Lexer $ fromText $ pack $ show number

bigInt :: Integer -> Number
bigInt number = Number $ Lexer $ fromText $ pack $ show number ++ "n"

token :: Prelude.String -> Lexer
token = Lexer . (<> fromString " ") . fromString

run :: Lexer -> Builder
run (Lexer tokens) = tokens
