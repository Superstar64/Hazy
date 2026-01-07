module Javascript.Printer.Tree.Block where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.StatementListItem (StatementListItem)
import Prelude hiding (print)

type Block :: Bool -> Bool -> Bool -> Type
newtype Block yield await return = Block Lexer

instance Print (Block yield await return) where
  print (Block ast) = ast

block :: [StatementListItem yield await return] -> Block yield await return
block statements = Block $ token "{" <> print statements <> token "}"
