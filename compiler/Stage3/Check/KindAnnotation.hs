module Stage3.Check.KindAnnotation where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDefinition as Stage2 (TypeDefinition (..))
import Stage3.Check.Context (Context)
import qualified Stage3.Temporary.Scheme as Unsolved.Scheme
import qualified Stage3.Temporary.Type as Type
import qualified Stage3.Temporary.Type as Unsolved.Type
import qualified Stage3.Tree.Type as Solved
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple (Type, simplify)

data KindAnnotation scope
  = Annotation {kind' :: !(Simple.Type scope)}
  | Inferred
  | Synonym
      { kind :: !(Strict.Maybe (Solved.Type scope)),
        kind' :: !(Simple.Type scope),
        definition :: !(Solved.Type (Local ':+ scope)),
        definition' :: !(Simple.Type (Local ':+ scope))
      }

check :: Context s scope -> Stage2.TypeDeclaration scope -> ST s (KindAnnotation scope)
check context declaration
  | position <- Stage2.position declaration,
    Stage2.Synonym {synonym, parameters} <- Stage2.definition declaration =
      do
        target <- Unify.fresh (Unify.typeWith Unify.large)
        annotation <- case declaration of
          Stage2.Inferred {} -> pure Inferred
          Stage2.Annotated {annotation} -> do
            universe <- Unify.fresh Unify.universe
            annotation <- Type.check context (Unify.typeWith universe) annotation
            annotation <- Type.solve context annotation
            pure Annotation {kind' = Simple.simplify annotation}
        let unused = error "unused parameter"
        (parameters, kind) <- TypeDeclaration.checkHead unused unused annotation (parameters, target)
        context <- pure $ Unsolved.Scheme.augment parameters context
        definition <- Unsolved.Type.check context (shift target) synonym
        kind' <- Unify.solve position kind
        definition <- Unsolved.Type.solve context definition
        let definition' = Simple.simplify definition
        pure Synonym {kind = Strict.Nothing, kind', definition, definition'}
check context declaration = case declaration of
  Stage2.Inferred {} -> pure Inferred
  Stage2.Annotated {annotation} -> do
    universe <- Unify.fresh Unify.universe
    annotation <- Type.check context (Unify.typeWith universe) annotation
    annotation <- Type.solve context annotation
    pure $ Annotation {kind' = Simple.simplify annotation}
