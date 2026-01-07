module Javascript.Printer.Tree.NamedExports where

import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.ExportsList (ExportsList)
import Prelude hiding (print)

newtype NamedExports = NamedExports Lexer

instance Print NamedExports where
  print (NamedExports ast) = ast

namedExports2 :: ExportsList -> NamedExports
namedExports2 exports = NamedExports $ token "{" <> print exports <> token "}"
