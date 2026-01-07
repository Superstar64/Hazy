module Javascript.Printer.Tree.ConditionalExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import {-# SOURCE #-} Javascript.Printer.Tree.AssignmentExpression (AssignmentExpression)
import Javascript.Printer.Tree.ShortCircuitExpression
  ( ShortCircuitExpression,
  )
import Prelude hiding (print)

type ConditionalExpression :: Bool -> Bool -> Bool -> Type
newtype ConditionalExpression inx yield await = ConditionalExpression Lexer

instance Print (ConditionalExpression inx yield await) where
  print (ConditionalExpression ast) = ast

conditionalExpression1 :: ShortCircuitExpression inx yield await -> ConditionalExpression inx yield await
conditionalExpression1 ast = ConditionalExpression $ print ast

conditionalExpression2 ::
  ShortCircuitExpression inx yield await ->
  AssignmentExpression 'True yield await ->
  AssignmentExpression inx yield await ->
  ConditionalExpression inx yield await
conditionalExpression2 condition valid invalid =
  ConditionalExpression $
    mconcat
      [ print condition,
        token "?",
        print valid,
        token ":",
        print invalid
      ]
