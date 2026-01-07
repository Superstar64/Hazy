module Javascript.Printer.Tree.MultiplicativeExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.ExponentiationExpression
  ( ExponentiationExpression,
  )
import Prelude hiding (print)

type MultiplicativeExpression :: Bool -> Bool -> Type
newtype MultiplicativeExpression yield await = MultiplicativeExpression Lexer

instance Print (MultiplicativeExpression yield await) where
  print (MultiplicativeExpression ast) = ast

multiplicativeExpression1 :: ExponentiationExpression yield await -> MultiplicativeExpression yield await
multiplicativeExpression1 ast = MultiplicativeExpression $ print ast
