module Javascript.Tree.Statement where

import Data.Text (Text)
import qualified Javascript.Printer as Printer
import Javascript.Tree.Expression (Expression)
import qualified Javascript.Tree.Expression as Expression

data Statement local where
  Const :: !Text -> Expression -> Statement local
  Let :: !Text -> Statement local
  Return :: Expression -> Statement 'True
  Expression :: Expression -> Statement local
  Import :: !Text -> !Text -> !Text -> Statement 'False
  Export :: !Text -> !Text -> Statement 'False
  If :: Expression -> [Statement 'True] -> Statement 'True
  Label :: !Text -> [Statement 'True] -> Statement 'True
  Break :: !Text -> Statement local

print :: Statement 'True -> Printer.StatementListItem yield await 'True
print = print' Local id

data Specific local a where
  Global :: (Printer.ModuleItem -> a) -> Specific 'False a
  Local :: Specific 'True a

print' ::
  Specific local a ->
  (Printer.StatementListItem yield await local -> a) ->
  Statement local ->
  a
print' specific lift = \case
  Const name body
    | const <- Printer.letOrConst2,
      body <- Expression.print body,
      name <- Printer.identifier name,
      binding <- Printer.lexicalBinding1 name $ Just body,
      bindings <- Printer.bindingList1 binding,
      lexicalDeclaration <- Printer.lexicalDeclaration const bindings,
      declaration <- Printer.declaration3 lexicalDeclaration,
      statementList <- Printer.statementListItem2 declaration ->
        lift statementList
  Let name
    | letx <- Printer.letOrConst1,
      name <- Printer.identifier name,
      binding <- Printer.lexicalBinding1 name Nothing,
      bindings <- Printer.bindingList1 binding,
      lexicalDeclaration <- Printer.lexicalDeclaration letx bindings,
      declaration <- Printer.declaration3 lexicalDeclaration,
      statementList <- Printer.statementListItem2 declaration ->
        lift statementList
  Return result
    | result <- Expression.print result,
      return <- Printer.returnStatement2 result,
      statement <- Printer.statement9 return,
      statementItem <- Printer.statementListItem1 statement ->
        lift statementItem
  Expression expression
    | expression <- Expression.print expression,
      statement <- Printer.statement4 expression,
      statementItem <- Printer.statementListItem1 statement ->
        lift statementItem
  Import target as from
    | Global lift <- specific,
      target <- Printer.string target,
      target <- Printer.moduleExportName2 target,
      as <- Printer.identifier as,
      specifier <- Printer.importSpecifier2 target as,
      lists <- Printer.importsList1 specifier,
      named <- Printer.namedImports2 lists,
      importx <- Printer.importClause3 named,
      from <- Printer.string from,
      from <- Printer.fromClause1 from,
      importx <- Printer.importDeclaration1 importx from Nothing,
      moduleItem <- Printer.moduleItem1 importx ->
        lift moduleItem
  Export source target
    | Global lift <- specific,
      -- todo this is wrong, import and export are reversed
      source <- Printer.identifier source,
      target <- Printer.string target,
      target <- Printer.moduleExportName2 target,
      specifier <- Printer.exportSpecifier2 source target,
      lists <- Printer.exportsList1 specifier,
      named <- Printer.namedExports2 lists,
      export <- Printer.exportDeclaration2 named,
      moduleItem <- Printer.moduleItem2 export ->
        lift moduleItem
  If condition valid
    | Local <- specific,
      condition <- Expression.print condition,
      valid <- Printer.block (map (print' Local id) valid),
      valid <- Printer.statement1 valid,
      ifx <- Printer.ifStatement2 condition valid,
      statement <- Printer.statement5 ifx,
      statement <- Printer.statementListItem1 statement ->
        lift statement
  Label name statements
    | Local <- specific,
      name <- Printer.identifier name,
      block <- Printer.block (map (print' Local id) statements),
      block <- Printer.statement1 block,
      labelled <- Printer.labelledItem1 block,
      labelled <- Printer.labelledStatement name labelled,
      statement <- Printer.statement11 labelled,
      statement <- Printer.statementListItem1 statement ->
        lift statement
  Break name
    | name <- Printer.identifier name,
      break <- Printer.breakStatement2 name,
      statement <- Printer.statement8 break,
      statement <- Printer.statementListItem1 statement ->
        lift statement
