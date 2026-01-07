module Javascript.Printer.Tree.ShiftExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.AdditiveExpression
  ( AdditivieExpression,
  )
import Prelude hiding (print)

type ShiftExpression :: Bool -> Bool -> Type
newtype ShiftExpression yield await = ShiftExpression Lexer

instance Print (ShiftExpression yield await) where
  print (ShiftExpression ast) = ast

shiftExpression1 :: AdditivieExpression yield await -> ShiftExpression yield await
shiftExpression1 ast = ShiftExpression $ print ast
