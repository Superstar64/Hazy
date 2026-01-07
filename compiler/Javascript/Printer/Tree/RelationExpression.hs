module Javascript.Printer.Tree.RelationExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.ShiftExpression (ShiftExpression)
import Prelude hiding (print)

type RelationalExpression :: Bool -> Bool -> Bool -> Type
newtype RelationalExpression inx yield await = RelationalExpression Lexer

instance Print (RelationalExpression inx yield await) where
  print (RelationalExpression ast) = ast

relationalExpression1 :: ShiftExpression yield await -> RelationalExpression inx yield await
relationalExpression1 ast = RelationalExpression $ print ast
