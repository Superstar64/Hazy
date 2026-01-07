module Javascript.Printer.Tree.ImportClause where

import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.NamedImports (NamedImports)
import Prelude hiding (print)

newtype ImportClause = ImportClause Lexer

instance Print ImportClause where
  print (ImportClause ast) = ast

importClause3 :: NamedImports -> ImportClause
importClause3 named = ImportClause $ print named
