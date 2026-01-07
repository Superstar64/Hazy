module Javascript.Printer.Tree.LiteralPropertyName where

import Javascript.Printer.Lexer (Identifier, Lexer, Print, print)
import Prelude hiding (print)

newtype LiteralPropertyName = LiteralPropertyName Lexer

instance Print LiteralPropertyName where
  print (LiteralPropertyName ast) = ast

literalPropertyName1 :: Identifier -> LiteralPropertyName
literalPropertyName1 name = LiteralPropertyName $ print name
