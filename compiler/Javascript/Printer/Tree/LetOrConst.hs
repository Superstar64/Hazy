module Javascript.Printer.Tree.LetOrConst where

import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Prelude hiding (print)

newtype LetOrConst = LetOrConst Lexer

letOrConst1 = LetOrConst $ token "let"

letOrConst2 = LetOrConst $ token "const"

instance Print LetOrConst where
  print (LetOrConst ast) = ast
