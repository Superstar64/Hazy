module Stage3.Tree.TypeDefinition2 where

import Stage1.Position (Position)
import Stage2.Stage (Check)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypeDefinition (TypeDefinition)

data TypeDefinition2 scope
  = !(Annotation scope) ::: !(TypeDefinition Check scope)
  deriving (Show)

infixr 5 :::

data Annotation scope
  = Annotated !(Type Position Check scope)
  | Inferred
  deriving (Show)
