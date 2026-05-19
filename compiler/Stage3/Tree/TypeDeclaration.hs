module Stage3.Tree.TypeDeclaration where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Layout (Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Stage (Check)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import Stage2.Tree.TypeDefinition (TypeDefinition (..))
import Stage2.Tree.TypeDefinition2 (Annotation (..), TypeDefinition2 (..))
import qualified Stage2.Tree.TypeDefinition2 as Stage2 (TypeDefinition2 (..))
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Temporary.TypeDefinition as Temporary.TypeDefinition
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type)

data LazyTypeDeclaration locality layout scope = !ConstructorIdentifier :^ TypeDeclaration locality layout scope
  deriving (Show)

infix 4 :^

type TypeDeclaration :: Locality -> Layout -> Environment -> Type
data TypeDeclaration locality layout scope
  = TypeDeclaration
  { name :: !ConstructorIdentifier,
    definition :: !(TypeDefinition2 locality layout Check scope),
    kind :: !(Simple.Type scope)
  }
  deriving (Show)

strict declaration@TypeDeclaration {name} = name :^ declaration

kind_ :: TypeDeclaration locality layout scope -> Simple.Type scope
kind_ = kind

check ::
  Context s scope ->
  Stage3.KindAnnotation scope ->
  Stage2.TypeDeclaration locality Normal scope ->
  ST s (TypeDeclaration locality Normal scope)
check
  _
  KindAnnotation.Synonym {kind, annotation', parameters, synonym}
  Stage2.TypeDeclaration {name} =
    case annotation' of
      Strict.Nothing ->
        pure
          TypeDeclaration
            { name,
              definition = Inferred ::: Synonym {parameters, synonym},
              kind
            }
      Strict.Just annotation ->
        pure
          TypeDeclaration
            { name,
              definition = Annotated annotation ::: Synonym {parameters, synonym},
              kind
            }
check
  context
  KindAnnotation.Inferred
  Stage2.TypeDeclaration {position, name, definition = _ Stage2.::: definition} = do
    kind <- Unify.fresh Unify.kind
    definition <- Temporary.TypeDefinition.check context kind definition
    kind <- Unify.solve position kind
    definition <- Temporary.TypeDefinition.solve context definition
    pure
      TypeDeclaration
        { name,
          definition = Inferred ::: definition,
          kind
        }
check
  context
  KindAnnotation.Annotation {annotation, kind}
  Stage2.TypeDeclaration {name, definition = _ Stage2.::: definition} = do
    definition <- Temporary.TypeDefinition.check context (Simple.Type.lift kind) definition
    definition <- Temporary.TypeDefinition.solve context definition
    pure
      TypeDeclaration
        { name,
          definition = Annotated annotation ::: definition,
          kind
        }
