module Stage3.Tree.TypeDeclaration where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Layout (Normal)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDefinition2 as Stage2 (TypeDefinition2 (..))
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Temporary.TypeDefinition as Temporary.TypeDefinition
import Stage3.Tree.TypeDefinition (TypeDefinition (..))
import Stage3.Tree.TypeDefinition2 (Annotation (..), TypeDefinition2 (..))
import qualified Stage3.Tree.TypeDefinition2 as TypeDefinition2
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type)

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope
  deriving (Show)

infix 4 :^

data TypeDeclaration scope
  = TypeDeclaration
  { name :: !ConstructorIdentifier,
    definition :: !(TypeDefinition2 scope)
  }
  deriving (Show)

strict declaration@TypeDeclaration {name} = name :^ declaration

kind_ :: TypeDeclaration scope -> Simple.Type scope
kind_ = \case
  TypeDeclaration {definition = annotation ::: _} -> TypeDefinition2.kind annotation

check ::
  Context s scope ->
  Stage3.KindAnnotation scope ->
  Stage2.TypeDeclaration locality Normal scope ->
  ST s (TypeDeclaration scope)
check
  _
  KindAnnotation.Synonym {kind, annotation', parameters, synonym}
  Stage2.TypeDeclaration {name} =
    case annotation' of
      Strict.Nothing ->
        pure
          TypeDeclaration
            { name,
              definition = Inferred {kind} ::: Synonym {parameters, synonym}
            }
      Strict.Just annotation ->
        pure
          TypeDeclaration
            { name,
              definition = Annotated {kind, annotation} ::: Synonym {parameters, synonym}
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
          definition = Inferred {kind} ::: definition
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
          definition = Annotated {kind, annotation} ::: definition
        }
