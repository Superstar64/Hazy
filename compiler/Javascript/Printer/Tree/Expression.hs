module Javascript.Printer.Tree.Expression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import {-# SOURCE #-} Javascript.Printer.Tree.AssignmentExpression (AssignmentExpression)
import Prelude hiding (print)

type Expression :: Bool -> Bool -> Bool -> Type
newtype Expression inx yield await = Expression Lexer

instance Print (Expression inx yield await) where
  print (Expression ast) = ast

expression1 :: AssignmentExpression inx yield await -> Expression inx yield await
expression1 ast = Expression (print ast)
