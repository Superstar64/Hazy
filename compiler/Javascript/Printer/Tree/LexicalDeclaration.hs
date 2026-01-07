module Javascript.Printer.Tree.LexicalDeclaration where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.BindingList (BindingList)
import Javascript.Printer.Tree.LetOrConst (LetOrConst)
import Prelude hiding (print)

type LexicalDeclaration :: Bool -> Bool -> Bool -> Type
newtype LexicalDeclaration inx yield await = LexicalDeclaration Lexer

instance Print (LexicalDeclaration inx yield await) where
  print (LexicalDeclaration ast) = ast

lexicalDeclaration :: LetOrConst -> BindingList inx yield await -> LexicalDeclaration inx yield await
lexicalDeclaration letOrConst bindings =
  LexicalDeclaration $ print letOrConst <> print bindings <> token ";"
