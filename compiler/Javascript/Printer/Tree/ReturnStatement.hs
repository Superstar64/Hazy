module Javascript.Printer.Tree.ReturnStatement where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.Expression (Expression)
import Prelude hiding (print)

type ReturnStatement :: Bool -> Bool -> Type
newtype ReturnStatement yield await = ReturnStatement Lexer

instance Print (ReturnStatement yield await) where
  print (ReturnStatement ast) = ast

returnStatement2 :: Expression 'True yield await -> ReturnStatement yield await
returnStatement2 result = ReturnStatement $ token "return" <> print result <> token ";"
