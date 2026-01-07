module Javascript.Printer.Tree.ExportDeclaration where

import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.NamedExports (NamedExports)
import Prelude hiding (print)

newtype ExportDeclaration = ExportDeclaration Lexer

instance Print ExportDeclaration where
  print (ExportDeclaration ast) = ast

exportDeclaration2 :: NamedExports -> ExportDeclaration
exportDeclaration2 named =
  ExportDeclaration $
    token "export" <> print named <> token ";"
