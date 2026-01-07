module Javascript.Printer.Tree.WithClause where

import Javascript.Printer.Lexer (Lexer, Print (print))

newtype WithClause = WithClause Lexer

instance Print WithClause where
  print (WithClause ast) = ast
