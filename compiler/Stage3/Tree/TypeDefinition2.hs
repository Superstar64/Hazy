module Stage3.Tree.TypeDefinition2 where

import Stage3.Tree.Type (Type)
import Stage3.Tree.TypeDefinition (TypeDefinition)
import qualified Stage4.Tree.Type as Simple (Type)

data TypeDefinition2 scope
  = !(Annotation scope) ::: !(TypeDefinition scope)
  deriving (Show)

infixr 5 :::

data Annotation scope
  = Annotated
      { kind :: !(Simple.Type scope),
        annotation :: !(Type scope)
      }
  | Inferred
      { kind :: !(Simple.Type scope)
      }
  deriving (Show)
