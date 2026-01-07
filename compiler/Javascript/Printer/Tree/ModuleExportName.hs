module Javascript.Printer.Tree.ModuleExportName where

import Javascript.Printer.Lexer (Lexer, Print (..), String)
import Prelude hiding (String, print)

newtype ModuleExportName = ModuleExportName Lexer

instance Print ModuleExportName where
  print (ModuleExportName ast) = ast

moduleExportName2 :: String -> ModuleExportName
moduleExportName2 ast = ModuleExportName $ print ast
