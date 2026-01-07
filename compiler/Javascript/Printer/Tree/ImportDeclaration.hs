module Javascript.Printer.Tree.ImportDeclaration where

import Javascript.Printer.Lexer (Lexer, Print (..), token)
import Javascript.Printer.Tree.FromClause (FromClause)
import Javascript.Printer.Tree.ImportClause (ImportClause)
import Javascript.Printer.Tree.WithClause (WithClause)
import Prelude hiding (print)

newtype ImportDeclaration = ImportDeclaration Lexer

instance Print ImportDeclaration where
  print (ImportDeclaration ast) = ast

importDeclaration1 :: ImportClause -> FromClause -> Maybe WithClause -> ImportDeclaration
importDeclaration1 importx from with =
  ImportDeclaration $
    mconcat
      [ token "import",
        print importx,
        print from,
        print with,
        token ";"
      ]
