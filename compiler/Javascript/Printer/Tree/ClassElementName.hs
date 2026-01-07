module Javascript.Printer.Tree.ClassElementName where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.PropertyName (PropertyName)
import Prelude hiding (print)

type ClassElementName :: Bool -> Bool -> Type
newtype ClassElementName yield await = ClassElementName Lexer

instance Print (ClassElementName yield await) where
  print (ClassElementName ast) = ast

classElementName1 :: PropertyName yield await -> ClassElementName yield await
classElementName1 ast = ClassElementName (print ast)
