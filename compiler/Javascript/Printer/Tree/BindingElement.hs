module Javascript.Printer.Tree.BindingElement where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.SingleNameBinding (SingleNameBinding)
import Prelude hiding (print)

type BindingElement :: Bool -> Bool -> Type
newtype BindingElement yield await = BindingElement Lexer

instance Print (BindingElement yield await) where
  print (BindingElement ast) = ast

bindingElement1 :: SingleNameBinding yield await -> BindingElement yield await
bindingElement1 ast = BindingElement $ print ast
