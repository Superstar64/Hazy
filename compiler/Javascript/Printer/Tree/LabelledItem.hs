module Javascript.Printer.Tree.LabelledItem where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import {-# SOURCE #-} Javascript.Printer.Tree.Statement (Statement)
import Prelude hiding (print)

type LabelledItem :: Bool -> Bool -> Bool -> Type
newtype LabelledItem yield await return = LabelledItem Lexer

instance Print (LabelledItem yield await return) where
  print (LabelledItem ast) = ast

labelledItem1 :: Statement yield await return -> LabelledItem yield await return
labelledItem1 ast = LabelledItem $ print ast
