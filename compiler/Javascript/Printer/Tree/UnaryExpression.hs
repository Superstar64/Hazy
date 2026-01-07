module Javascript.Printer.Tree.UnaryExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.UpdateExpression
  ( UpdateExpression,
  )
import Prelude hiding (print)

type UnaryExpression :: Bool -> Bool -> Type
newtype UnaryExpression yield await = UnaryExpression Lexer

instance Print (UnaryExpression yield await) where
  print (UnaryExpression ast) = ast

unaryExpression1 :: UpdateExpression yield await -> UnaryExpression yield await
unaryExpression1 ast = UnaryExpression $ print ast
