module Javascript.Printer.Tree.ModuleItem where

import Javascript.Printer.Lexer (Lexer, Print (..))
import Javascript.Printer.Tree.ExportDeclaration (ExportDeclaration)
import Javascript.Printer.Tree.ImportDeclaration (ImportDeclaration)
import Javascript.Printer.Tree.StatementListItem
  ( StatementListItem,
  )
import Prelude hiding (print)

newtype ModuleItem = ModuleItem Lexer

instance Print ModuleItem where
  print (ModuleItem ast) = ast

moduleItem1 :: ImportDeclaration -> ModuleItem
moduleItem1 ast = ModuleItem $ print ast

moduleItem2 :: ExportDeclaration -> ModuleItem
moduleItem2 ast = ModuleItem $ print ast

moduleItem3 :: StatementListItem 'False 'True 'False -> ModuleItem
moduleItem3 ast = ModuleItem $ print ast
