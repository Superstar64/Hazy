module Javascript.Printer.Tree.ObjectLiteral where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print, print, token)
import Javascript.Printer.Tree.PropertyDefinitionList (PropertyDefinitionList)
import Prelude hiding (print)

type ObjectLiteral :: Bool -> Bool -> Type
newtype ObjectLiteral yield await = ObjectLiteral Lexer

instance Print (ObjectLiteral yield await) where
  print (ObjectLiteral ast) = ast

objectLiteral1 :: ObjectLiteral yield await
objectLiteral1 = ObjectLiteral $ token "{" <> token "}"

objectLiteral2 :: PropertyDefinitionList yield await -> ObjectLiteral yield await
objectLiteral2 list = ObjectLiteral $ token "{" <> print list <> token "}"
