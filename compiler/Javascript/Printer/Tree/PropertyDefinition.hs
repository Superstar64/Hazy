module Javascript.Printer.Tree.PropertyDefinition where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print, print, token)
import {-# SOURCE #-} Javascript.Printer.Tree.AssignmentExpression (AssignmentExpression)
import Javascript.Printer.Tree.MethodDefinition (MethodDefinition)
import Javascript.Printer.Tree.PropertyName (PropertyName)
import Prelude hiding (print)

type PropertyDefinition :: Bool -> Bool -> Type
newtype PropertyDefinition yield await = PropertyDefinition Lexer

instance Print (PropertyDefinition yield await) where
  print (PropertyDefinition ast) = ast

propertyDefinition3 ::
  PropertyName yield await ->
  AssignmentExpression 'True yield await ->
  PropertyDefinition yield await
propertyDefinition3 name expression =
  PropertyDefinition $ print name <> token ":" <> print expression

propertyDefinition4 :: MethodDefinition yield await -> PropertyDefinition yield await
propertyDefinition4 ast = PropertyDefinition $ print ast
