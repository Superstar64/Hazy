module Stage3.Check.KindAnnotation where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Type as Solved (Type)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDefinition as Stage2 (TypeDefinition (Synonym, parameters, synonym))
import qualified Stage2.Tree.TypeDefinition2 as Stage2 (Annotation (..), TypeDefinition2 (..))
import qualified Stage2.Tree.TypePattern
import qualified Stage2.Tree.TypePattern as Solved (TypePattern)
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (TypePattern))
import Stage3.Check.Context (Context)
import qualified Stage3.Simple.Type as Simple (lift)
import qualified Stage3.Temporary.Scheme as Unsolved.Scheme
import qualified Stage3.Temporary.Type as Type
import qualified Stage3.Temporary.Type as Unsolved.Type
import qualified Stage3.Temporary.TypePattern as Unsolved
import qualified Stage3.Temporary.TypePattern as Unsolved.TypePattern
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type, simplify)

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

check :: Context s scope -> Stage2.TypeDeclaration locality Group Resolve scope -> ST s (KindAnnotation scope)
check
  context
  Stage2.TypeDeclaration
    { position,
      definition = annotation Stage2.::: Stage2.Synonym {synonym, parameters}
    } =
    do
      let fresh Stage2.TypePattern {name, position} = do
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
        Stage2.InferredAcyclic -> pure Strict.Nothing
        Stage2.Annotated annotation -> do
          universe <- Unify.fresh Unify.universe
          annotation <- Type.check context (Unify.typeWith universe) annotation
          annotation <- Type.solve context annotation
          Unify.unify context position kind (Simple.lift $ Simple.simplify annotation)
          pure $ Strict.Just annotation
      context <- pure $ Unsolved.Scheme.augment parameters context
      synonym <- Unsolved.Type.check context (shift target) synonym
      kind <- Unify.solve position kind
      parameters <- traverse Unsolved.TypePattern.solve parameters
      synonym <- Unsolved.Type.solve context synonym
      pure Synonym {annotation', kind, parameters, synonym}
check context Stage2.TypeDeclaration {definition} = case definition of
  Stage2.Annotated annotation Stage2.::: _ -> do
    universe <- Unify.fresh Unify.universe
    annotation <- Type.check context (Unify.typeWith universe) annotation
    annotation <- Type.solve context annotation
    pure $ Annotation {annotation, kind = Simple.simplify annotation}
  _ -> pure Inferred
