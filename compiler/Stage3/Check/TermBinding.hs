module Stage3.Check.TermBinding where

import Control.Monad.ST (ST)
import Stage2.Scope (Environment (..), GroupTerm)
import Stage2.Shift (Shift (..))
import Stage2.Stage (Check)
import {-# SOURCE #-} Stage2.Tree.Declaration (Declaration)
import {-# SOURCE #-} qualified Stage2.Tree.Declaration as Declaration
import Stage3.Check.TypeAnnotation (Annotation (..), TypeAnnotation (..))
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import {-# SOURCE #-} qualified Stage3.Temporary.Declaration as Temporary
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple (Scheme)

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
