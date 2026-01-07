module Javascript.Printer.Tree.ImportSpecifier where

import Javascript.Printer.Lexer (Identifier, Lexer, Print (..), token)
import Javascript.Printer.Tree.ModuleExportName (ModuleExportName)
import Prelude hiding (print)

newtype ImportSpecifier = ImportSpecifier Lexer

instance Print ImportSpecifier where
  print (ImportSpecifier ast) = ast

importSpecifier2 :: ModuleExportName -> Identifier -> ImportSpecifier
importSpecifier2 moduleName binding =
  ImportSpecifier $
    print moduleName <> token "as" <> print binding
