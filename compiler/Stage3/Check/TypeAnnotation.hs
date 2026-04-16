{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import qualified Stage2.Tree.Scheme as Stage2 (Scheme (..))
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..), TermDeclaration' (..))
import {-# SOURCE #-} Stage3.Check.Context (Context)
import {-# SOURCE #-} qualified Stage3.Temporary.Scheme as Scheme (check, solve)
import {-# SOURCE #-} Stage3.Tree.Scheme (Scheme)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

data Annotation scope = Annotation
  { annotation :: !(Scheme scope),
    annotation' :: !(Simple.Scheme scope)
  }

data GlobalTypeAnnotation scope
  = GlobalAnnotation !(Annotation scope)
  | GlobalInferred

data LocalTypeAnnotation s scope
  = LocalAnnotation !(Annotation scope)
  | LocalInferred !(Unify.Type s scope)

checkAnnotation :: Context s scope -> Stage2.Scheme Position scope -> ST s (Annotation scope)
checkAnnotation context annotation = do
  annotation <- Scheme.check context annotation
  annotation <- Scheme.solve context annotation
  let annotation' = Simple.simplify annotation
  pure $ Annotation {annotation, annotation'}

checkGlobal :: Context s scope -> Stage2.TermDeclaration scope -> ST s (GlobalTypeAnnotation scope)
checkGlobal context Stage2.TermDeclaration {declaration} =
  case declaration of
    Stage2.Annotated {annotation} -> GlobalAnnotation <$> checkAnnotation context annotation
    Stage2.Inferred {} -> pure GlobalInferred

checkLocal :: Context s scope -> Stage2.TermDeclaration scope -> ST s (LocalTypeAnnotation s scope)
checkLocal context Stage2.TermDeclaration {declaration} =
  case declaration of
    Stage2.Annotated {annotation} -> LocalAnnotation <$> checkAnnotation context annotation
    Stage2.Inferred {} -> do
      typex <- Unify.fresh Unify.typex
      pure $ LocalInferred typex
