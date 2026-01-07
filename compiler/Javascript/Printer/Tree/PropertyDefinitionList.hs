module Javascript.Printer.Tree.PropertyDefinitionList where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print, print, token)
import Javascript.Printer.Tree.PropertyDefinition (PropertyDefinition)
import Prelude hiding (print)

type PropertyDefinitionList :: Bool -> Bool -> Type
newtype PropertyDefinitionList yield await = PropertyDefinitionList Lexer

instance Print (PropertyDefinitionList yield await) where
  print (PropertyDefinitionList ast) = ast

propertyDefinitionList1 :: PropertyDefinition yield await -> PropertyDefinitionList yield await
propertyDefinitionList1 ast = PropertyDefinitionList $ print ast

propertyDefinitionList2 ::
  PropertyDefinitionList yield await ->
  PropertyDefinition yield await ->
  PropertyDefinitionList yield await
propertyDefinitionList2 list definition =
  PropertyDefinitionList $ print list <> token "," <> print definition
