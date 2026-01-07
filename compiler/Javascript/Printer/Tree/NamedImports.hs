module Javascript.Printer.Tree.NamedImports where

import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.ImportsList (ImportsList)
import Prelude hiding (print)

newtype NamedImports = NamedImports Lexer

instance Print NamedImports where
  print (NamedImports ast) = ast

namedImports2 :: ImportsList -> NamedImports
namedImports2 imports = NamedImports $ token "{" <> print imports <> token "}"
