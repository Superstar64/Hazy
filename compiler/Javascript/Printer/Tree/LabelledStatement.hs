module Javascript.Printer.Tree.LabelledStatement where

import Data.Kind (Type)
import Javascript.Printer.Lexer
  ( Identifier,
    Lexer,
    Print (..),
    token,
  )
import Javascript.Printer.Tree.LabelledItem (LabelledItem)
import Prelude hiding (print)

type LabelledStatement :: Bool -> Bool -> Bool -> Type
newtype LabelledStatement yield await return = LabelledStatement Lexer

instance Print (LabelledStatement yield await return) where
  print (LabelledStatement ast) = ast

labelledStatement ::
  Identifier ->
  LabelledItem yield await return ->
  LabelledStatement yield await return
labelledStatement name statement = LabelledStatement $ print name <> token ":" <> print statement
