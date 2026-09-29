module Javascript.Printer.Tree.MemberExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Identifier, Lexer, Print (..), token)
import Javascript.Printer.Tree.Expression (Expression)
import Javascript.Printer.Tree.PrimaryExpression
  ( PrimaryExpression,
  )
import Prelude hiding (print)

type MemberExpression :: Bool -> Bool -> Type
newtype MemberExpression yield await = MemberExpression Lexer

instance Print (MemberExpression yield await) where
  print (MemberExpression ast) = ast

memberExpression1 :: PrimaryExpression yield await -> MemberExpression yield await
memberExpression1 ast = MemberExpression $ print ast

memberExpression2 :: MemberExpression yield await -> Expression 'True yield await -> MemberExpression yield await
memberExpression2 array index =
  MemberExpression $ print array <> token "[" <> print index <> token "]"

memberExpression3 :: MemberExpression yield await -> Identifier -> MemberExpression yield await
memberExpression3 expression field =
  MemberExpression $ print expression <> token "." <> print field
