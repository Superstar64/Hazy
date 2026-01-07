module Javascript.Printer.Tree.AdditiveExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.MultiplicativeExpression
  ( MultiplicativeExpression,
  )
import Prelude hiding (print)

type AdditivieExpression :: Bool -> Bool -> Type
newtype AdditivieExpression yield await = AdditivieExpression Lexer

instance Print (AdditivieExpression yield await) where
  print (AdditivieExpression ast) = ast

additivieExpression1 :: MultiplicativeExpression yield await -> AdditivieExpression yield await
additivieExpression1 ast = AdditivieExpression $ print ast
