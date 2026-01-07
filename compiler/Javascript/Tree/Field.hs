module Javascript.Tree.Field where

import Data.Text (Text)
import qualified Javascript.Printer as Printer
import {-# SOURCE #-} Javascript.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Javascript.Tree.Expression as Expression
import {-# SOURCE #-} Javascript.Tree.Statement (Statement)
import {-# SOURCE #-} qualified Javascript.Tree.Statement as Statement

data Field
  = Literal {literal :: Expression}
  | Method
      { definition :: [Statement 'True]
      }

print :: Text -> Field -> Printer.PropertyDefinition yield await
print name = \case
  Literal {literal}
    | field <- Expression.print literal,
      name <- Printer.identifier name,
      name <- Printer.literalPropertyName1 name,
      name <- Printer.propertyName1 name ->
        Printer.propertyDefinition3 name field
  Method {definition}
    | name <- Printer.identifier name,
      name <- Printer.literalPropertyName1 name,
      name <- Printer.propertyName1 name,
      name <- Printer.classElementName1 name,
      parameters <- Printer.formalParameters1,
      definition <- Statement.print <$> definition,
      method <- Printer.methodDefinition1 name parameters definition ->
        Printer.propertyDefinition4 method
