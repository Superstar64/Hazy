module Javascript.Printer.Tree.Literal where

import Javascript.Printer.Lexer (Lexer, Number, Print (print), String)
import Prelude hiding (String, print)

newtype Literal = Literal Lexer

instance Print Literal where
  print (Literal ast) = ast

-- todo implement proper literal ast
literal3 :: Number -> Literal
literal3 = Literal . print

literal4 :: String -> Literal
literal4 = Literal . print
