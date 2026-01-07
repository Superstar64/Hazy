module Javascript.Printer.Tree.FromClause where

import Javascript.Printer.Lexer (Lexer, Print (..), String, token)
import Prelude hiding (String, print)

newtype FromClause = FromClause Lexer

instance Print FromClause where
  print (FromClause ast) = ast

fromClause1 :: String -> FromClause
fromClause1 modulex = FromClause $ token "from" <> print modulex
