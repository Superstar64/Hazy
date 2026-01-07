module Javascript.Printer.Tree.Declaration where

import Data.Kind (Type)
import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.LexicalDeclaration
  ( LexicalDeclaration,
  )
import Prelude hiding (print)

type Declaration :: Bool -> Bool -> Type
newtype Declaration yield await = Declaration Lexer

instance Print (Declaration yield await) where
  print (Declaration ast) = ast

declaration3 :: LexicalDeclaration 'True yield await -> Declaration yield await
declaration3 ast = Declaration $ print ast
