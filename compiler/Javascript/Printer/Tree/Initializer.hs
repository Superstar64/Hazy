module Javascript.Printer.Tree.Initializer where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..), token)
import {-# SOURCE #-} Javascript.Printer.Tree.AssignmentExpression (AssignmentExpression)
import Prelude hiding (print)

type Initializer :: Bool -> Bool -> Bool -> Type
newtype Initializer inx yield await = Initializer Lexer

initializer1 :: AssignmentExpression inx yield await -> Initializer inx yield await
initializer1 assignment = Initializer $ token "=" <> print assignment

instance Print (Initializer inx yield await) where
  print (Initializer ast) = ast
