module Javascript.Printer.Tree.BitwiseORExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.BitwiseXORExpression
  ( BitwiseXORExpression,
  )
import Prelude hiding (print)

type BitwiseORExpression :: Bool -> Bool -> Bool -> Type
newtype BitwiseORExpression inx yield await = BitwiseORExpression Lexer

instance Print (BitwiseORExpression inx yield await) where
  print (BitwiseORExpression ast) = ast

bitwiseORExpression1 :: BitwiseXORExpression inx yield await -> BitwiseORExpression inx yield await
bitwiseORExpression1 ast = BitwiseORExpression $ print ast
