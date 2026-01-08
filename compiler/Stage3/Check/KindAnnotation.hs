module Stage3.Check.KindAnnotation where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import qualified Stage2.Tree.TypeDeclaration as Stage2
  ( TypeDeclaration
      ( Synonym,
        position,
        synonym
      ),
    annotation,
    parameters,
  )
import Stage3.Check.Context (Context)
import qualified Stage3.Simple.Type as Simple (Type, simplify)
import qualified Stage3.Synonym as Synonym
import qualified Stage3.Temporary.Scheme as Unsolved.Scheme
import qualified Stage3.Temporary.Type as Type
import qualified Stage3.Temporary.Type as Unsolved.Type
import qualified Stage3.Tree.Type as Solved
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import {-# SOURCE #-} qualified Stage3.Unify as Unify

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
check context Stage2.Synonym {position, synonym, parameters, annotation} =
  do
    target <- Unify.fresh (Unify.typeWith Unify.large)
    annotation <- case annotation of
      Strict.Nothing -> pure Inferred
      Strict.Just annotation -> do
        universe <- Unify.fresh Unify.universe
        annotation <- Type.check context (Unify.typeWith universe) annotation
        let synonym = Synonym.fromProper context
        annotation <- Type.solve synonym annotation
        pure Annotation {kind' = Simple.simplify annotation}
    let unused = error "unused parameter"
    (parameters, kind) <- TypeDeclaration.checkHead unused unused annotation (parameters, target)
    context <- pure $ Unsolved.Scheme.augment parameters context
    definition <- Unsolved.Type.check context (shift target) synonym
    kind' <- Unify.solve position kind
    let simplify = Synonym.fromProper context
    definition <- Unsolved.Type.solve simplify definition
    let definition' = Simple.simplify definition
    pure Synonym {kind = Strict.Nothing, kind', definition, definition'}
check context declaration = case Stage2.annotation declaration of
  Strict.Nothing -> pure Inferred
  Strict.Just annotation -> do
    universe <- Unify.fresh Unify.universe
    annotation <- Type.check context (Unify.typeWith universe) annotation
    let synonym = Synonym.fromProper context
    annotation <- Type.solve synonym annotation
    pure $ Annotation {kind' = Simple.simplify annotation}
