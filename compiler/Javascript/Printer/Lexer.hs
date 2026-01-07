module Javascript.Printer.Lexer
  ( Lexer,
    Print (..),
    Identifier,
    identifier,
    String,
    string,
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

instance Print Int where
  print = Lexer . fromText . pack . show

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

token :: Prelude.String -> Lexer
token = Lexer . (<> fromString " ") . fromString

run :: Lexer -> Builder
run (Lexer tokens) = tokens
