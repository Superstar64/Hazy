module Javascript.Printer.Tree.ShortCircuitExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.LogicalORExpression
  ( LogicalORExpression,
  )
import Prelude hiding (print)

type ShortCircuitExpression :: Bool -> Bool -> Bool -> Type
newtype ShortCircuitExpression inx yield await = ShortCircuitExpression Lexer

instance Print (ShortCircuitExpression inx yield await) where
  print (ShortCircuitExpression ast) = ast

shortCircuitExpression1 :: LogicalORExpression inx yield await -> ShortCircuitExpression inx yield await
shortCircuitExpression1 ast = ShortCircuitExpression $ print ast
