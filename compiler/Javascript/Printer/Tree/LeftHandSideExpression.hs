module Javascript.Printer.Tree.LeftHandSideExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.CallExpression (CallExpression)
import Javascript.Printer.Tree.NewExpression (NewExpression)
import Prelude hiding (print)

type LeftHandSideExpression :: Bool -> Bool -> Type
newtype LeftHandSideExpression yield await = LeftHandSideExpression Lexer

instance Print (LeftHandSideExpression yield await) where
  print (LeftHandSideExpression ast) = ast

leftHandSideExpression1 :: NewExpression yield await -> LeftHandSideExpression yield await
leftHandSideExpression1 ast = LeftHandSideExpression $ print ast

leftHandSideExpression2 :: CallExpression yield await -> LeftHandSideExpression yield await
leftHandSideExpression2 ast = LeftHandSideExpression $ print ast
