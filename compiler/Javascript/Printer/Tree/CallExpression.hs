module Javascript.Printer.Tree.CallExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print (..), token)
import Javascript.Printer.Tree.Arguments (Arguments)
import Javascript.Printer.Tree.MemberExpression (MemberExpression)
import Prelude hiding (print)

type CallExpression :: Bool -> Bool -> Type
newtype CallExpression yield await = CallExpression Lexer

instance Print (CallExpression yield await) where
  print (CallExpression ast) = ast

-- skip implementing CoverCallExpressionAndAsyncArrowHead
callExpression1 :: MemberExpression yield await -> Arguments yield await -> CallExpression yield await
callExpression1 expression arguments =
  CallExpression $ print expression <> print arguments

callExpression4 :: CallExpression yield await -> Arguments yield await -> CallExpression yield await
callExpression4 expression arguments =
  CallExpression $ print expression <> print arguments

callExpression6 :: CallExpression yield await -> Identifier -> CallExpression yield await
callExpression6 expression field =
  CallExpression $ print expression <> token "." <> print field
