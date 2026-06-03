module Stage3.Tree.TypeDeclaration (TypeDeclaration (..), kind', check, lazy) where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector as Vector
import Data.Vector.Strict (toLazy)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Link.Type as Type
import Stage2.Layout (Group)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.TypeDeclaration (TypeDeclaration (..), kind', lazy)
import Stage2.Tree.TypeDefinition (Alias (..), Inject (..), TypeDefinition (..), assumeInject)
import Stage2.Tree.TypeDefinition2 (Annotation (..), Element (..), Set (..), TypeDefinition2 (..), Types (..))
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import Stage3.Check.Context (Context (..), groupTypeBindings)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.KindAnnotation as Stage3
import qualified Stage3.Simple.Type as Simple.Type
import qualified Stage3.Temporary.TypeDefinition as Temporary.TypeDefinition
import qualified Stage3.Temporary.TypeDefinition as TypeDefinition
import qualified Stage3.Temporary.TypeDefinition2 as Temporary
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple

check ::
  Context s scope ->
  (Type.Link locality -> Int -> ST s (Simple.Type scope)) ->
  Stage3.KindAnnotation scope ->
  TypeDeclaration locality Group Resolve scope ->
  ST s (TypeDeclaration locality Group Check scope)
check _ _ annotation TypeDeclaration {position, name, constructorNames, definition = _ ::: Synonym {}}
  | KindAnnotation.Synonym {kind, annotation', parameters, synonym} <- annotation = case annotation' of
      Strict.Nothing ->
        pure
          TypeDeclaration
            { position,
              name,
              constructorNames,
              definition = InferredAcyclic ::: Synonym {parameters, synonym, alias = Alias},
              kind = Solved kind
            }
      Strict.Just annotation ->
        pure
          TypeDeclaration
            { position,
              name,
              constructorNames,
              definition = Annotated annotation ::: Synonym {parameters, synonym, alias = Alias},
              kind = Solved kind
            }
  | otherwise = error "bad synonym"
check context linked annotation TypeDeclaration {position, name, constructorNames, definition} =
  case definition of
    Annotated {} ::: definition
      | KindAnnotation.Annotation {annotation, kind} <- annotation,
        Inject <- assumeInject definition -> do
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
      | otherwise -> error "bad annotation"
    _ :::: Set set -> do
      fresh <- Vector.replicateM (length set) (Unify.fresh Unify.kind)
      let context' =
            groupTypeBindings
              (TypeDefinition2.position <$> toLazy set)
              (TypeDefinition2.label <$> toLazy set)
              fresh
              context
      set <-
        flip Strict.Vector.imapM set $
          \index
           Element {element, position, name, constructorNames, link} -> do
              let typex = fresh Vector.! index
              element <- TypeDefinition.check context' (shift typex) element
              pure $ Temporary.Element {element, typex, position, name, constructorNames, link}
      set <- traverse (Temporary.solveElement context') set
      kinds <- traverse (Unify.solve position) fresh
      let kind = Vector.head kinds
      pure
        TypeDeclaration
          { position,
            name,
            constructorNames,
            definition = Solved (Types $ Strict.Vector.fromLazy kinds) :::: Set set,
            kind = Solved kind
          }
    Link link id -> do
      kind <- linked link id
      pure
        TypeDeclaration
          { position,
            name,
            constructorNames,
            definition = Link link id,
            kind = Solved kind
          }
