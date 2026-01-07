module Javascript.Printer.Tree.AssignmentExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.ArrowFunction (ArrowFunction)
import Javascript.Printer.Tree.ConditionalExpression
  ( ConditionalExpression,
  )
import Javascript.Printer.Tree.LeftHandSideExpression (LeftHandSideExpression)
import Prelude hiding (print)

type AssignmentExpression :: Bool -> Bool -> Bool -> Type
newtype AssignmentExpression inx yield await = AssignmentExpression Lexer

instance Print (AssignmentExpression inx yield await) where
  print (AssignmentExpression ast) = ast

assignmentExpression1 :: ConditionalExpression inx yield await -> AssignmentExpression inx yield await
assignmentExpression1 ast = AssignmentExpression $ print ast

assignmentExpression3 :: ArrowFunction inx yield await -> AssignmentExpression inx yield await
assignmentExpression3 ast = AssignmentExpression $ print ast

assignmentExpression5 ::
  LeftHandSideExpression yield await ->
  AssignmentExpression inx yield await ->
  AssignmentExpression inx yield await
assignmentExpression5 target value =
  AssignmentExpression $ print target <> token "=" <> print value
