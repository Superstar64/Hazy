module Javascript.Printer.Tree.LogicalANDExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.BitwiseORExpression
  ( BitwiseORExpression,
  )
import Prelude hiding (print)

type LogicalANDExpression :: Bool -> Bool -> Bool -> Type
newtype LogicalANDExpression inx yield await = LogicalANDExpression Lexer

instance Print (LogicalANDExpression inx yield await) where
  print (LogicalANDExpression ast) = ast

logicialANDExpression1 :: BitwiseORExpression inx yield await -> LogicalANDExpression inx yield await
logicialANDExpression1 ast = LogicalANDExpression $ print ast
