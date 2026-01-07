module Javascript.Printer.Tree.Arguments where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.ArgumentList (ArgumentList)
import Prelude hiding (print)

type Arguments :: Bool -> Bool -> Type
newtype Arguments yield await = Arguments Lexer

instance Print (Arguments yield await) where
  print (Arguments ast) = ast

arguments1 :: Arguments yield await
arguments1 = Arguments $ token "(" <> token ")"

arguments2 :: ArgumentList yield await -> Arguments yield await
arguments2 list = Arguments $ token "(" <> print list <> token ")"
