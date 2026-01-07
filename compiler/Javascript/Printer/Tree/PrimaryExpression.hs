module Javascript.Printer.Tree.PrimaryExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print (..), token)
import Javascript.Printer.Tree.Expression (Expression)
import Javascript.Printer.Tree.Literal (Literal)
import Javascript.Printer.Tree.ObjectLiteral (ObjectLiteral)
import Prelude hiding (print)

type PrimaryExpression :: Bool -> Bool -> Type
newtype PrimaryExpression yield await = PrimaryExpression Lexer

instance Print (PrimaryExpression yield await) where
  print (PrimaryExpression ast) = ast

primaryExpression1 :: PrimaryExpression yield await
primaryExpression1 = PrimaryExpression $ token "this"

primaryExpression2 :: Identifier -> PrimaryExpression yield await
primaryExpression2 name = PrimaryExpression $ print name

primaryExpression3 :: Literal -> PrimaryExpression yield await
primaryExpression3 ast = PrimaryExpression (print ast)

primaryExpression5 :: ObjectLiteral yield await -> PrimaryExpression yield await
primaryExpression5 ast = PrimaryExpression (print ast)

-- skip implementing `CoverParenthesizedExpressionAndArrowParameterList`
primaryExpression13 :: Expression 'True yield await -> PrimaryExpression yield await
primaryExpression13 expression =
  PrimaryExpression $
    token "(" <> print expression <> token ")"
