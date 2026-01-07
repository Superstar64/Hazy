module Javascript.Printer.Tree.ArrowFunction where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (print), token)
import Javascript.Printer.Tree.ArrowParameters (ArrowParameters)
import Javascript.Printer.Tree.ConciseBody (ConciseBody)
import Prelude hiding (print)

type ArrowFunction :: Bool -> Bool -> Bool -> Type
newtype ArrowFunction inx yield await = ArrowFunction Lexer

instance Print (ArrowFunction inx yield await) where
  print (ArrowFunction ast) = ast

arrowFunction ::
  ArrowParameters yield await ->
  ConciseBody inx ->
  ArrowFunction inx yield await
arrowFunction parameters concise =
  ArrowFunction $ print parameters <> token "=>" <> print concise
