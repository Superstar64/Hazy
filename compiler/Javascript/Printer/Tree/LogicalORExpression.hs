module Javascript.Printer.Tree.LogicalORExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.LogicalANDExpression
  ( LogicalANDExpression,
  )
import Prelude hiding (print)

type LogicalORExpression :: Bool -> Bool -> Bool -> Type
newtype LogicalORExpression inx yield await = LogicalORExpression Lexer

instance Print (LogicalORExpression inx yield await) where
  print (LogicalORExpression ast) = ast

logicialORExpression1 :: LogicalANDExpression inx yield await -> LogicalORExpression inx yield await
logicialORExpression1 ast = LogicalORExpression $ print ast
