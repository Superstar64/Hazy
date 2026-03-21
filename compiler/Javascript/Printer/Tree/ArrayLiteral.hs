module Javascript.Printer.Tree.ArrayLiteral where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.ElementList (ElementList)
import Javascript.Printer.Tree.Elision (Elision)
import Prelude hiding (print)

type ArrayLiteral :: Bool -> Bool -> Type
newtype ArrayLiteral yield await = ArrayLiteral Lexer

instance Print (ArrayLiteral yield await) where
  print (ArrayLiteral ast) = ast

arrayLiteral1 :: Maybe Elision -> ArrayLiteral yield await
arrayLiteral1 elision = ArrayLiteral $ mconcat [token "[", print elision, token "]"]

arrayLiteral2 :: ElementList yield await -> ArrayLiteral yield await
arrayLiteral2 elements = ArrayLiteral $ mconcat [token "[", print elements, token "]"]
