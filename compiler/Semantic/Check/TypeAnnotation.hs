{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Temporary.Scheme as Scheme (check, solve)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Scheme (Scheme)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

checkAnnotation :: Context s scope -> Scheme Position Resolve scope -> ST s (Scheme Position Check scope)
checkAnnotation context annotation = do
  annotation <- Scheme.check context annotation
  annotation <- Unify.runSolve $ Scheme.solve context annotation
  pure $ annotation
