module Javascript.Printer.Tree.ImportsList where

import Javascript.Printer.Lexer (Lexer, Print (print))
import Javascript.Printer.Tree.ImportSpecifier (ImportSpecifier)
import Prelude hiding (print)

newtype ImportsList = ImportsList Lexer

instance Print ImportsList where
  print (ImportsList ast) = ast

importsList1 :: ImportSpecifier -> ImportsList
importsList1 ast = ImportsList $ print ast
