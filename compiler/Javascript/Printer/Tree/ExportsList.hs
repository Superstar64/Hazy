module Javascript.Printer.Tree.ExportsList where

import Javascript.Printer.Lexer (Lexer, Print (print))
import Javascript.Printer.Tree.ExportSpecifier (ExportSpecifier)
import Prelude hiding (print)

newtype ExportsList = ExportsList Lexer

instance Print ExportsList where
  print (ExportsList ast) = ast

exportsList1 :: ExportSpecifier -> ExportsList
exportsList1 ast = ExportsList $ print ast
