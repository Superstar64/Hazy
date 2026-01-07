module Javascript.Printer.Tree.BitwiseXORExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.BitwiseANDExpression
  ( BitwiseANDExpression,
  )
import Prelude hiding (print)

type BitwiseXORExpression :: Bool -> Bool -> Bool -> Type
newtype BitwiseXORExpression inx yield await = BitwiseXORExpression Lexer

instance Print (BitwiseXORExpression inx yield await) where
  print (BitwiseXORExpression ast) = ast

bitwiseXORExpression1 :: BitwiseANDExpression inx yield await -> BitwiseXORExpression inx yield await
bitwiseXORExpression1 ast = BitwiseXORExpression $ print ast
