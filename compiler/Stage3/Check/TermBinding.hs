module Stage3.Check.TermBinding where

import Control.Monad.ST (ST)
import Stage2.Layout (Normal)
import Stage2.Shift (Shift (..))
import Stage2.Stage (Check)
import {-# SOURCE #-} Stage2.Tree.Declaration (Declaration)
import {-# SOURCE #-} qualified Stage2.Tree.Declaration as Declaration
import Stage3.Check.TypeAnnotation (Annotation (..), GlobalTypeAnnotation (..), LocalTypeAnnotation (..))
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple (Scheme)

data Type s scope
  = Wobbly !(Unify.Type s scope)
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
    (ST s (GlobalTypeAnnotation scope))
    (ST s (Declaration locality Normal Check scope)) ->
  TermBinding s scope
rigid Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  Rigid <$> case annotation of
    GlobalAnnotation Annotation {annotation'} -> pure annotation'
    GlobalInferred -> Declaration.typex' <$> content

wobbly ::
  Functor.Annotated
    name
    (ST s (LocalTypeAnnotation s scope))
    b ->
  TermBinding s scope
wobbly Functor.Annotated {meta} = TermBinding $ do
  annotation <- meta
  case annotation of
    LocalAnnotation Annotation {annotation'} -> do
      pure (Rigid annotation')
    LocalInferred typex -> pure $ Wobbly typex
