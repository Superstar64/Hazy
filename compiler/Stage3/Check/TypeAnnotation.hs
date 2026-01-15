{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import Error (unsupportedFeaturePatternLetBinds)
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..))
import {-# SOURCE #-} Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
import {-# SOURCE #-} qualified Stage3.Temporary.Scheme as Scheme (check, solve)
import {-# SOURCE #-} Stage3.Tree.Scheme (Scheme)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

data TypeAnnotation inferred scope
  = Annotation
      { annotation :: !(Scheme scope),
        annotation' :: !(Simple.Scheme scope)
      }
  | Inferred !inferred

check ::
  Term0.Index scope ->
  inferred ->
  Context s scope ->
  Stage2.TermDeclaration scope ->
  ST s (TypeAnnotation inferred scope)
check _ _ context Stage2.Manual {annotation} = do
  annotation <- Scheme.check context annotation
  let simplify = Synonym.fromProper context
  annotation <- Scheme.solve simplify annotation
  let annotation' = Simple.simplify annotation
  pure $ Annotation {annotation, annotation'}
check _ auto _ Stage2.Auto {} = pure $ Inferred auto
check _ _ _ Stage2.Share {position} = unsupportedFeaturePatternLetBinds position

instanciate :: TypeAnnotation () scope -> ST s (TypeAnnotation (Unify.Type s scope) scope)
instanciate = \case
  Inferred () -> Inferred <$> Unify.fresh Unify.typex
  Annotation {annotation, annotation'} -> pure $ Annotation {annotation, annotation'}
