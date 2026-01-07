module Javascript.Printer.Tree.BitwiseANDExpression where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.EqualityExpression
  ( EqualityExpression,
  )
import Prelude hiding (print)

type BitwiseANDExpression :: Bool -> Bool -> Bool -> Type
newtype BitwiseANDExpression inx yield await = BitwiseANDExpression Lexer

instance Print (BitwiseANDExpression inx yield await) where
  print (BitwiseANDExpression ast) = ast

bitwiseANDExpression1 :: EqualityExpression inx yield await -> BitwiseANDExpression inx yield await
bitwiseANDExpression1 ast = BitwiseANDExpression $ print ast
