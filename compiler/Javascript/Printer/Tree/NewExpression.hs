module Javascript.Printer.Tree.NewExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.MemberExpression
  ( MemberExpression,
  )
import Prelude hiding (print)

type NewExpression :: Bool -> Bool -> Type
newtype NewExpression yield await = NewExpression Lexer

instance Print (NewExpression yield await) where
  print (NewExpression ast) = ast

newExpression1 :: MemberExpression yield await -> NewExpression yield await
newExpression1 ast = NewExpression $ print ast
