{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Scheme as Simple
import {-# SOURCE #-} Semantic.Check.Context (Context)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Scheme as Scheme (check, solve)
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Declaration as Semantic (Declaration (..))
import qualified Semantic.Tree.Definition4 as Semantic (Annotation (..), Definition4 (..))
import Semantic.Tree.Scheme (Scheme)
import qualified Semantic.Tree.Scheme as Semantic (Scheme (..))
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Annotation scope = Annotation
  { annotation :: !(Scheme Position Check scope),
    annotation' :: !(Simple.Scheme scope)
  }

data TypeAnnotation scope
  = Annotated !(Annotation scope)
  | Inferred

checkAnnotation :: Context s scope -> Semantic.Scheme Position Resolve scope -> ST s (Annotation scope)
checkAnnotation context annotation = do
  annotation <- Scheme.check context annotation
  annotation <- Unify.runSolve $ Scheme.solve context annotation
  let annotation' = Simple.simplify annotation
  pure $ Annotation {annotation, annotation'}

check :: Context s scope -> Semantic.Declaration locality Group Resolve scope -> ST s (TypeAnnotation scope)
check context Semantic.Declaration {definition} = case definition of
  Semantic.Annotated annotation Semantic.::: _ -> Annotated <$> checkAnnotation context annotation
  _ -> pure Inferred
