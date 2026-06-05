module Semantic.Check.TermBinding where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Scheme as Simple (Scheme)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Declaration as Temporary
import Semantic.Check.TypeAnnotation (Annotation (..), TypeAnnotation (..))
import Semantic.Scope (Environment (..), GroupTerm)
import Semantic.Shift (Shift (..))
import Semantic.Stage (Check)
import {-# SOURCE #-} Semantic.Tree.Declaration (Declaration)
import {-# SOURCE #-} qualified Semantic.Tree.Declaration as Declaration
import {-# SOURCE #-} qualified Semantic.Unify as Unify

data Type s scope
  = Wobbly !(Unify.Scheme s scope)
  | Rigid !(Simple.Scheme scope)

newtype TermBinding s scope = TermBinding
  {typex :: ST s (Type s scope)}

instance Shift (Type s) where
  shift = \case
    Wobbly typex -> Wobbly (shift typex)
    Rigid typex -> Rigid (shift typex)

instance Shift (TermBinding s) where
  shift TermBinding {typex} = TermBinding {typex = fmap shift typex}

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
group = TermBinding . pure . Wobbly . Unify.monoScheme . shift
