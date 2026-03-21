module Javascript.Printer.Tree.Elision where

import Javascript.Printer.Lexer (Lexer, Print (..))

newtype Elision = Elision Lexer

instance Print Elision where
  print (Elision ast) = ast
