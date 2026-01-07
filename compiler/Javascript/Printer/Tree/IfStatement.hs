module Javascript.Printer.Tree.IfStatement where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.Expression (Expression)
import {-# SOURCE #-} Javascript.Printer.Tree.Statement (Statement)
import Prelude hiding (print)

type IfStatement :: Bool -> Bool -> Bool -> Type
newtype IfStatement yield await return = IfStatement Lexer

instance Print (IfStatement yield await return) where
  print (IfStatement ast) = ast

-- todo deal with lookahead
ifStatement2 ::
  Expression 'True yield await ->
  Statement yield await return ->
  IfStatement yield await return
ifStatement2 expression valid =
  IfStatement $
    mconcat
      [ token "if",
        token "(",
        print expression,
        token ")",
        print valid
      ]
