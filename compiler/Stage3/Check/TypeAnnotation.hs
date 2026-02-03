{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Check.TypeAnnotation where

import Control.Monad.ST (ST)
import Error (unsupportedFeaturePatternLetBinds)
import Stage1.Position (Position)
import qualified Stage2.Tree.Scheme as Stage2 (Scheme (..))
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..))
import {-# SOURCE #-} Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
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

data AnyAnnotation s scope
  = AnyAnnotation !(Annotation scope)
  | Global
  | Local !(Unify.Type s scope)

global :: GlobalTypeAnnotation scope -> AnyAnnotation s scope
global = \case
  GlobalAnnotation annotation -> AnyAnnotation annotation
  GlobalInferred -> Global

local :: LocalTypeAnnotation s scope -> AnyAnnotation s scope
local = \case
  LocalAnnotation annotation -> AnyAnnotation annotation
  LocalInferred typex -> Local typex

checkAnnotation :: Context s scope -> Stage2.Scheme Position scope -> ST s (Annotation scope)
checkAnnotation context annotation = do
  annotation <- Scheme.check context annotation
  let simplify = Synonym.fromProper context
  annotation <- Scheme.solve simplify annotation
  let annotation' = Simple.simplify annotation
  pure $ Annotation {annotation, annotation'}

checkGlobal :: Context s scope -> Stage2.TermDeclaration scope -> ST s (GlobalTypeAnnotation scope)
checkGlobal context Stage2.Manual {annotation} =
  GlobalAnnotation <$> checkAnnotation context annotation
checkGlobal _ Stage2.Auto {} = pure GlobalInferred
checkGlobal _ Stage2.Share {position} = unsupportedFeaturePatternLetBinds position

checkLocal :: Context s scope -> Stage2.TermDeclaration scope -> ST s (LocalTypeAnnotation s scope)
checkLocal context Stage2.Manual {annotation} =
  LocalAnnotation <$> checkAnnotation context annotation
checkLocal _ Stage2.Auto {} = do
  typex <- Unify.fresh Unify.typex
  pure $ LocalInferred typex
checkLocal _ Stage2.Share {position} = unsupportedFeaturePatternLetBinds position
