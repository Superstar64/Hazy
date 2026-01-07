module Javascript.Tree.Module where

import qualified Javascript.Printer as Printer
import Javascript.Tree.Statement (Statement)
import qualified Javascript.Tree.Statement as Statement

print :: [Statement 'False] -> [Printer.ModuleItem]
print = map print'

print' :: Statement 'False -> Printer.ModuleItem
print' = Statement.print' (Statement.Global id) Printer.moduleItem3
