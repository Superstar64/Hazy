module Javascript.Printer.Tree.ExportSpecifier where

import Javascript.Printer.Lexer (Identifier, Lexer, Print (..), token)
import Javascript.Printer.Tree.ModuleExportName (ModuleExportName)
import Prelude hiding (print)

newtype ExportSpecifier = ExportSpecifier Lexer

instance Print ExportSpecifier where
  print (ExportSpecifier ast) = ast

exportSpecifier2 :: Identifier -> ModuleExportName -> ExportSpecifier
exportSpecifier2 moduleName binding =
  ExportSpecifier $
    print moduleName <> token "as" <> print binding
