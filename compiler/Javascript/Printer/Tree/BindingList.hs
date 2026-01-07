module Javascript.Printer.Tree.BindingList where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print, print, token)
import Javascript.Printer.Tree.LexicalBinding (LexicalBinding)
import Prelude hiding (print)

type BindingList :: Bool -> Bool -> Bool -> Type
newtype BindingList inx yield await = BindingList Lexer

instance Print (BindingList inx yield await) where
  print (BindingList ast) = ast

bindingList1 :: LexicalBinding inx yield await -> BindingList inx yield await
bindingList1 ast = BindingList (print ast)

bindingList2 ::
  BindingList inx yield await ->
  LexicalBinding inx yield await ->
  BindingList inx yield await
bindingList2 list binding = BindingList $ print list <> token "," <> print binding
