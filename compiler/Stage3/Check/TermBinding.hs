module Stage3.Check.TermBinding where

import Control.Monad.ST (ST)
import Stage2.Shift (Shift (..))
import Stage3.Check.TypeAnnotation (TypeAnnotation (Annotation, Inferred))
import qualified Stage3.Check.TypeAnnotation as Annotation (annotation')
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import {-# SOURCE #-} qualified Stage3.Simple.Scheme as Simple (Scheme)
import {-# SOURCE #-} Stage3.Tree.TermDeclaration (TermDeclaration)
import {-# SOURCE #-} qualified Stage3.Tree.TermDeclaration as TermDeclaration
import {-# SOURCE #-} qualified Stage3.Unify as Unify

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
  Functor.Annotated name (ST s (TypeAnnotation () scope)) (ST s (TermDeclaration scope)) ->
  TermBinding s scope
rigid Functor.Annotated {meta, content} = TermBinding $ do
  annotation <- meta
  Rigid <$> case annotation of
    Annotation {annotation'} -> pure annotation'
    Inferred () -> TermDeclaration.simple <$> content

wobbly ::
  Functor.Annotated
    name
    (ST s (TypeAnnotation (Unify.Type s scope) scope))
    b ->
  TermBinding s scope
wobbly Functor.Annotated {meta} = TermBinding $ do
  annotation <- meta
  case annotation of
    Annotation {annotation'} -> do
      pure (Rigid annotation')
    Inferred typex -> pure $ Wobbly typex
