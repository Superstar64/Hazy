{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity (..))
import Data.Void (Void)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Scheme as Scheme (check, solve)
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Declaration as Semantic (Declaration (..))
import qualified Semantic.Tree.Definition4 as Semantic (Annotation (..), Definition4 (..))
import Semantic.Tree.Scheme (Scheme)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data TypeAnnotation scope
  = Annotated !(Scheme Position Check scope)
  | Inferred

checkAnnotation :: Context s scope -> Scheme Position Resolve scope -> ST s (Scheme Position Check scope)
checkAnnotation context annotation = do
  annotation <- Scheme.check context annotation
  annotation <- Unify.runSolve $ Scheme.solve context annotation
  pure $ annotation

check ::
  Context s scope ->
  Semantic.Declaration Identity Void Identity locality Group Resolve scope ->
  ST s (TypeAnnotation scope)
check context Semantic.Declaration {definition} = case definition of
  Semantic.Annotated annotation Semantic.::: _ -> do
    annotation <- checkAnnotation context annotation
    pure $ Annotated annotation
  _ -> pure $ Inferred
