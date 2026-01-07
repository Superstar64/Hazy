module Javascript.Printer.Tree.FormalParameterList where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.BindingElement (BindingElement)
import Prelude hiding (print)

type FormalParameterList :: Bool -> Bool -> Type
newtype FormalParameterList yield await = FormalParameterList Lexer

instance Print (FormalParameterList yield await) where
  print (FormalParameterList ast) = ast

formalParameterList1 :: BindingElement yield await -> FormalParameterList yield await
formalParameterList1 ast = FormalParameterList $ print ast

formalParameterList2 :: FormalParameterList yield await -> BindingElement yield await -> FormalParameterList yield await
formalParameterList2 list next = FormalParameterList $ print list <> token "," <> print next
