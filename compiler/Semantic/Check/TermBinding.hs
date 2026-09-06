module Semantic.Check.TermBinding where

import Control.Monad.ST (ST)
import qualified Core.Tree.Forall as Core (mono)
import qualified Core.Tree.Forall as Forall
import qualified Core.Tree.Forall as Simple (Forall)
import qualified Core.Type.Functor as Core (mapLogical, shiftLogical)
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import Semantic.Check.TypeAnnotation (TypeAnnotation (..))
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), GroupTerm)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import {-# SOURCE #-} Semantic.Tree.Declaration (Declaration, typex')
import {-# SOURCE #-} qualified Semantic.Tree.Declaration as Declaration
import qualified Semantic.Unify as Unify

data Type s scope
  = Wobbly !(Unify.Forall s scope)
  | Rigid !(Simple.Forall scope)

newtype TermBinding s scope = TermBinding
  {typex :: ST s (Type s scope)}

instance Shift0.Functor (Type s) where
  map category = \case
    Wobbly typex -> Wobbly (Core.mapLogical category typex)
    Rigid typex -> Rigid (Shift0.map category typex)

instance Shift0.Functor (TermBinding s) where
  map category TermBinding {typex} = TermBinding {typex = fmap (Shift0.map category) typex}

rigid ::
  Functor.Annotated
    name
    (ST s (TypeAnnotation scope))
    (ST s (Declaration Identity Void locality Identity layout Check scope)) ->
  TermBinding s scope
rigid Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  Rigid <$> case annotation of
    Annotated annotation -> pure (Forall.simplify annotation)
    Inferred -> Declaration.typex' <$> content

wobbly ::
  Functor.Annotated
    name
    (ST s (TypeAnnotation scope))
    (ST s (Declaration (Unify.Solve s) (Unify.Logical s scope) locality Identity Group Check scope)) ->
  TermBinding s scope
wobbly Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  case annotation of
    Annotated annotation -> do
      pure (Rigid $ Forall.simplify annotation)
    Inferred -> do
      Wobbly . typex' <$> content

group :: Unify.Type s scopes -> TermBinding s (GroupTerm ':+ scopes)
group = TermBinding . pure . Wobbly . Core.mono . Core.shiftLogical
