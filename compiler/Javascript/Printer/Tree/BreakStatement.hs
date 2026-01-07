module Javascript.Printer.Tree.BreakStatement where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print (..), token)
import Prelude hiding (print)

type BreakStatement :: Bool -> Bool -> Type
newtype BreakStatement yield await = BreakStatement Lexer

instance Print (BreakStatement yield await) where
  print (BreakStatement ast) = ast

breakStatement2 :: Identifier -> BreakStatement yield await
breakStatement2 label = BreakStatement $ token "break" <> print label <> token ";"
