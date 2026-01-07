module Javascript.Printer.Tree.ArgumentList where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import {-# SOURCE #-} Javascript.Printer.Tree.AssignmentExpression (AssignmentExpression)
import Prelude hiding (print)

type ArgumentList :: Bool -> Bool -> Type
newtype ArgumentList yield await = ArgumentList Lexer

instance Print (ArgumentList yield await) where
  print (ArgumentList ast) = ast

argumentList1 :: AssignmentExpression 'True yield await -> ArgumentList yield await
argumentList1 ast = ArgumentList $ print ast

argumentList3 ::
  ArgumentList yield await ->
  AssignmentExpression 'True yield await ->
  ArgumentList yield await
argumentList3 list expression =
  ArgumentList $ print list <> token "," <> print expression
