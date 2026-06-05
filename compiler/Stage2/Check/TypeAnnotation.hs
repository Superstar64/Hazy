{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Group)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Declaration as Stage2 (Declaration (..))
import qualified Stage2.Tree.Definition4 as Stage2 (Annotation (..), Definition4 (..))
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Stage2 (Scheme (..))
import {-# SOURCE #-} Stage2.Check.Context (Context)
import {-# SOURCE #-} qualified Stage2.Check.Temporary.Scheme as Scheme (check, solve)
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

data Annotation scope = Annotation
  { annotation :: !(Scheme Position Check scope),
    annotation' :: !(Simple.Scheme scope)
  }

data TypeAnnotation scope
  = Annotated !(Annotation scope)
  | Inferred

checkAnnotation :: Context s scope -> Stage2.Scheme Position Resolve scope -> ST s (Annotation scope)
checkAnnotation context annotation = do
  annotation <- Scheme.check context annotation
  annotation <- Unify.runSolve $ Scheme.solve context annotation
  let annotation' = Simple.simplify annotation
  pure $ Annotation {annotation, annotation'}

check :: Context s scope -> Stage2.Declaration locality Group Resolve scope -> ST s (TypeAnnotation scope)
check context Stage2.Declaration {definition} = case definition of
  Stage2.Annotated annotation Stage2.::: _ -> Annotated <$> checkAnnotation context annotation
  _ -> pure Inferred
