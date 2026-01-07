module Javascript.Printer.Tree.EqualityExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.RelationExpression
  ( RelationalExpression,
  )
import Prelude hiding (print)

type EqualityExpression :: Bool -> Bool -> Bool -> Type
newtype EqualityExpression inx yield await = EqualityExpression Lexer

instance Print (EqualityExpression inx yield await) where
  print (EqualityExpression ast) = ast

equalityExpression1 :: RelationalExpression inx yield await -> EqualityExpression inx yield await
equalityExpression1 ast = EqualityExpression $ print ast

equalityExpression4 ::
  EqualityExpression inx yield await ->
  RelationalExpression inx yield await ->
  EqualityExpression inx yield await
equalityExpression4 left right = EqualityExpression $ print left <> token "===" <> print right
