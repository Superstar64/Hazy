module Stage3.Tree.TypeDeclaration (TypeDeclaration (..), kind', check, lazy) where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Stage2.Layout (Normal)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.TypeDeclaration (TypeDeclaration (..), kind', lazy)
import Stage2.Tree.TypeDefinition (TypeDefinition (..))
import Stage2.Tree.TypeDefinition2 (Annotation (..), TypeDefinition2 (..))
import qualified Stage2.Tree.TypeDefinition2 as Stage2 (TypeDefinition2 (..))
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Temporary.TypeDefinition as Temporary.TypeDefinition
import qualified Stage3.Unify as Unify

check ::
  Context s scope ->
  Stage3.KindAnnotation scope ->
  TypeDeclaration locality Normal Resolve scope ->
  ST s (TypeDeclaration locality Normal Check scope)
check
  _
  KindAnnotation.Synonym {kind, annotation', parameters, synonym}
  TypeDeclaration {position, name, constructorNames} =
    case annotation' of
      Strict.Nothing ->
        pure
          TypeDeclaration
            { position,
              name,
              constructorNames,
              definition = Inferred ::: Synonym {parameters, synonym},
              kind = Solved kind
            }
      Strict.Just annotation ->
        pure
          TypeDeclaration
            { position,
              name,
              constructorNames,
              definition = Annotated annotation ::: Synonym {parameters, synonym},
              kind = Solved kind
            }
check
  context
  KindAnnotation.Inferred
  TypeDeclaration {position, name, constructorNames, definition = _ Stage2.::: definition} = do
    kind <- Unify.fresh Unify.kind
    definition <- Temporary.TypeDefinition.check context kind definition
    kind <- Unify.solve position kind
    definition <- Temporary.TypeDefinition.solve context definition
    pure
      TypeDeclaration
        { position,
          name,
          constructorNames,
          definition = Inferred ::: definition,
          kind = Solved kind
        }
check
  context
  KindAnnotation.Annotation {annotation, kind}
  TypeDeclaration {position, name, constructorNames, definition = _ Stage2.::: definition} = do
    definition <- Temporary.TypeDefinition.check context (Simple.Type.lift kind) definition
    definition <- Temporary.TypeDefinition.solve context definition
    pure
      TypeDeclaration
        { position,
          name,
          constructorNames,
          definition = Annotated annotation ::: definition,
          kind = Solved kind
        }
