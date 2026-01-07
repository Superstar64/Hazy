module Javascript.Printer.Tree.UpdateExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.LeftHandSideExpression
  ( LeftHandSideExpression,
  )
import Prelude hiding (print)

type UpdateExpression :: Bool -> Bool -> Type
newtype UpdateExpression yield await = UpdateExpression Lexer

instance Print (UpdateExpression yield await) where
  print (UpdateExpression ast) = ast

updateExpression1 :: LeftHandSideExpression yield await -> UpdateExpression yield await
updateExpression1 ast = UpdateExpression $ print ast
