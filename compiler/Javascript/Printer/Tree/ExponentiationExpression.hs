module Javascript.Printer.Tree.ExponentiationExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.UnaryExpression (UnaryExpression)
import Prelude hiding (print)

type ExponentiationExpression :: Bool -> Bool -> Type
newtype ExponentiationExpression yield await = ExponentiationExpression Lexer

instance Print (ExponentiationExpression yield await) where
  print (ExponentiationExpression ast) = ast

exponentiationExpression1 :: UnaryExpression yield await -> ExponentiationExpression yield await
exponentiationExpression1 ast = ExponentiationExpression $ print ast
