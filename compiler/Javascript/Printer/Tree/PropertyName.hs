module Javascript.Printer.Tree.PropertyName where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print, print)
import Javascript.Printer.Tree.LiteralPropertyName (LiteralPropertyName)
import Prelude hiding (print)

type PropertyName :: Bool -> Bool -> Type
newtype PropertyName yield await = PropertyName Lexer

instance Print (PropertyName yield await) where
  print (PropertyName ast) = ast

propertyName1 :: LiteralPropertyName -> PropertyName yield await
propertyName1 ast = PropertyName (print ast)
