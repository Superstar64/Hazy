module Javascript.Printer.Tree.ConciseBody where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (print), token)
import {-# SOURCE #-} Javascript.Printer.Tree.StatementListItem (StatementListItem)
import Prelude hiding (print)

type ConciseBody :: Bool -> Type
newtype ConciseBody inx = ConciseBody Lexer

instance Print (ConciseBody inx) where
  print (ConciseBody ast) = ast

conciseBody2 :: [StatementListItem 'False 'False 'True] -> ConciseBody inx
conciseBody2 statements = ConciseBody $ token "{" <> print statements <> token "}"
