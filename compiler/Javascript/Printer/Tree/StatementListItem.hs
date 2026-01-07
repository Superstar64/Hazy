module Javascript.Printer.Tree.StatementListItem where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.Declaration (Declaration)
import {-# SOURCE #-} Javascript.Printer.Tree.Statement (Statement)
import Prelude hiding (print)

type StatementListItem :: Bool -> Bool -> Bool -> Type
newtype StatementListItem yield await return = StatementListItem Lexer

instance Print (StatementListItem yield await return) where
  print (StatementListItem ast) = ast

statementListItem1 :: Statement yield await return -> StatementListItem yield await return
statementListItem1 ast = StatementListItem $ print ast

statementListItem2 :: Declaration yield await -> StatementListItem yield await return
statementListItem2 ast = StatementListItem $ print ast
