module Javascript.Printer.Tree.ElementList where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import {-# SOURCE #-} Javascript.Printer.Tree.AssignmentExpression (AssignmentExpression)
import Javascript.Printer.Tree.Elision (Elision)
import Prelude hiding (print)

type ElementList :: Bool -> Bool -> Type
newtype ElementList yield await = ElementList Lexer

instance Print (ElementList yield await) where
  print (ElementList ast) = ast

elementList1 :: Maybe Elision -> AssignmentExpression 'True yield await -> ElementList yield await
elementList1 elision expression = ElementList $ print elision <> print expression

elementList3 ::
  ElementList yield await ->
  Maybe Elision ->
  AssignmentExpression 'True yield await ->
  ElementList yield await
elementList3 list elision expression =
  ElementList $
    mconcat
      [ print list,
        token ",",
        print elision,
        print expression
      ]
