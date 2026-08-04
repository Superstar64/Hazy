module Semantic.Check.TermBinding where

import Control.Monad.ST (ST)
import qualified Core.Tree.Forall as Core (mono)
import qualified Core.Tree.Forall as Simple (Forall)
import qualified Core.Type.Functor as Core (mapLogical, shiftLogical)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Declaration as Temporary
import Semantic.Check.TypeAnnotation (Annotation (..), TypeAnnotation (..))
import Semantic.Scope (Environment (..), GroupTerm)
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import {-# SOURCE #-} Semantic.Tree.Declaration (Declaration)
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
    (ST s (Declaration locality layout Check scope)) ->
  TermBinding s scope
rigid Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  Rigid <$> case annotation of
    Annotated Annotation {annotation'} -> pure annotation'
    Inferred -> Declaration.typex' <$> content

wobbly ::
  Functor.Annotated
    name
    (ST s (TypeAnnotation scope))
    (ST s (Temporary.Declaration locality s scope)) ->
  TermBinding s scope
wobbly Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  case annotation of
    Annotated Annotation {annotation'} -> do
      pure (Rigid annotation')
    Inferred -> do
      Wobbly . Temporary.typex' <$> content

group :: Unify.Type s scopes -> TermBinding s (GroupTerm ':+ scopes)
group = TermBinding . pure . Wobbly . Core.mono . Core.shiftLogical
