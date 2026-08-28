module Semantic.Check.Go.TypeDeclaration (TypeDeclaration (..), kind', check, lazy) where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Simple
import Core.Type.Functor (shiftLogical)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector as Vector
import Data.Vector.Strict (toLazy)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..), groupTypeBindings)
import qualified Semantic.Check.KindAnnotation as KindAnnotation
import qualified Semantic.Check.KindAnnotation as Semantic
import qualified Semantic.Check.Temporary.TypeDefinition as Temporary.TypeDefinition
import qualified Semantic.Check.Temporary.TypeDefinition as TypeDefinition
import qualified Semantic.Index.Link.Type as Type
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Synonym as Synonym
import Semantic.Tree.TypeDeclaration (TypeDeclaration (..), kind', lazy)
import Semantic.Tree.TypeDefinition2 (Annotation (..), TypeDefinition2 (..))
import Semantic.Tree.TypeGroup (Element (..), Set (..), TypeGroup (..), Types (..))
import qualified Semantic.Tree.TypeGroup as TypeGroup
import qualified Semantic.Unify as Unify

check ::
  Context s scope ->
  (Type.Link locality -> Int -> ST s (Simple.Type scope)) ->
  Semantic.KindAnnotation scope ->
  TypeDeclaration locality Group Resolve scope ->
  ST s (TypeDeclaration locality Group Check scope)
check _ _ annotation TypeDeclaration {position, name, constructorNames, definition = Synonym {}}
  | KindAnnotation.Synonym {kind, annotation', parameters, synonym} <- annotation = case annotation' of
      Strict.Nothing ->
        pure
          TypeDeclaration
            { position,
              name,
              constructorNames,
              definition = Synonym Inferred Synonym.Synonym {parameters, synonym},
              kind = Solved kind
            }
      Strict.Just annotation ->
        pure
          TypeDeclaration
            { position,
              name,
              constructorNames,
              definition = Synonym (Annotated annotation) Synonym.Synonym {parameters, synonym},
              kind = Solved kind
            }
  | otherwise = error "bad synonym"
check context linked annotation TypeDeclaration {position, name, constructorNames, definition} =
  case definition of
    Annotated {} ::: definition
      | KindAnnotation.Annotation {annotation, kind} <- annotation -> do
          definition <- Temporary.TypeDefinition.check context (logicalType kind) definition
          definition <- Unify.runSolve $ Temporary.TypeDefinition.solve context definition
          pure
            TypeDeclaration
              { position,
                name,
                constructorNames,
                definition = Annotated annotation ::: definition,
                kind = Solved kind
              }
      | otherwise -> error "bad annotation"
    Group (_ :::: Set set) -> do
      fresh <- Vector.replicateM (length set) (Unify.fresh Core.kind)
      let context' =
            groupTypeBindings
              (TypeGroup.position <$> toLazy set)
              (TypeGroup.label <$> toLazy set)
              fresh
              context
      set <-
        flip Strict.Vector.imapM set $
          \index
           Element {element, position, name, constructorNames, link} -> do
              let typex = fresh Vector.! index
              element <- TypeDefinition.check context' (shiftLogical typex) element
              pure $ do
                element <- TypeDefinition.solve context' element
                typex <- Unify.solve position typex
                pure Element {element, typex = Solved typex, position, name, constructorNames, link}
      set <- Unify.runSolve $ sequence set
      kinds <- Unify.runSolve $ traverse (Unify.solve position) fresh
      let kind = Vector.head kinds
      pure
        TypeDeclaration
          { position,
            name,
            constructorNames,
            definition = Group $ Solved (Types $ Strict.Vector.fromLazy kinds) :::: Set set,
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
