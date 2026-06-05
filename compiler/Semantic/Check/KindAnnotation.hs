module Semantic.Check.KindAnnotation where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Simple (Type, simplify)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Simple.Type as Simple (lift)
import qualified Semantic.Check.Temporary.Scheme as Unsolved.Scheme
import qualified Semantic.Check.Temporary.Type as Type
import qualified Semantic.Check.Temporary.Type as Unsolved.Type
import qualified Semantic.Check.Temporary.TypePattern as Unsolved
import qualified Semantic.Check.Temporary.TypePattern as Unsolved.TypePattern
import Semantic.Layout (Group)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Type as Solved (Type)
import qualified Semantic.Tree.TypeDeclaration as Semantic (TypeDeclaration (..))
import qualified Semantic.Tree.TypeDefinition as Semantic (TypeDefinition (Synonym, parameters, synonym))
import qualified Semantic.Tree.TypeDefinition2 as Semantic (Annotation (..), TypeDefinition2 (..))
import qualified Semantic.Tree.TypePattern
import qualified Semantic.Tree.TypePattern as Semantic (TypePattern (TypePattern))
import qualified Semantic.Tree.TypePattern as Solved (TypePattern)
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data KindAnnotation scope
  = Annotation
      { annotation :: !(Solved.Type Position Check scope),
        kind :: !(Simple.Type scope)
      }
  | Inferred
  | Synonym
      { annotation' :: !(Strict.Maybe (Solved.Type Position Check scope)),
        kind :: !(Simple.Type scope),
        parameters :: !(Strict.Vector (Solved.TypePattern Position Check scope)),
        synonym :: !(Solved.Type Position Check (Local ':+ scope))
      }

check :: Context s scope -> Semantic.TypeDeclaration locality Group Resolve scope -> ST s (KindAnnotation scope)
check
  context
  Semantic.TypeDeclaration
    { position,
      definition = annotation Semantic.::: Semantic.Synonym {synonym, parameters}
    } =
    do
      let fresh Semantic.TypePattern {name, position} = do
            level <- Unify.fresh Unify.universe
            typex <- Unify.fresh (Unify.typeWith level)
            pure
              Unsolved.TypePattern
                { name,
                  typex,
                  position
                }
      parameters <- traverse fresh parameters
      target <- Unify.fresh (Unify.typeWith Unify.large)
      let kind = foldr (Unify.function . Unsolved.TypePattern.typex) target parameters
      annotation' <- case annotation of
        Semantic.InferredAcyclic -> pure Strict.Nothing
        Semantic.Annotated annotation -> do
          universe <- Unify.fresh Unify.universe
          annotation <- Type.check context (Unify.typeWith universe) annotation
          annotation <- Unify.runSolve $ Type.solve context annotation
          Unify.unify context position kind (Simple.lift $ Simple.simplify annotation)
          pure $ Strict.Just annotation
      context <- pure $ Unsolved.Scheme.augment parameters context
      synonym <- Unsolved.Type.check context (shift target) synonym
      kind <- Unify.runSolve $ Unify.solve position kind
      parameters <- Unify.runSolve $ traverse Unsolved.TypePattern.solve parameters
      synonym <- Unify.runSolve $ Unsolved.Type.solve context synonym
      pure Synonym {annotation', kind, parameters, synonym}
check context Semantic.TypeDeclaration {definition} = case definition of
  Semantic.Annotated annotation Semantic.::: _ -> do
    universe <- Unify.fresh Unify.universe
    annotation <- Type.check context (Unify.typeWith universe) annotation
    annotation <- Unify.runSolve $ Type.solve context annotation
    pure $ Annotation {annotation, kind = Simple.simplify annotation}
  _ -> pure Inferred
