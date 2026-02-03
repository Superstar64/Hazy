module Stage3.Check.TermBinding where

import Control.Monad.ST (ST)
import Stage2.Shift (Shift (..))
import Stage3.Check.TypeAnnotation (Annotation (..), GlobalTypeAnnotation (..), LocalTypeAnnotation (..))
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import {-# SOURCE #-} Stage3.Tree.TermDeclaration (TermDeclaration)
import {-# SOURCE #-} qualified Stage3.Tree.TermDeclaration as TermDeclaration
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
  Functor.Annotated name (ST s (GlobalTypeAnnotation scope)) (ST s (TermDeclaration scope)) ->
  TermBinding s scope
rigid Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  Rigid <$> case annotation of
    GlobalAnnotation Annotation {annotation'} -> pure annotation'
    GlobalInferred -> TermDeclaration.simple <$> content

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
