module Semantic.Unify.Builtin where

import Core.Tree.Evidence (EvidenceF (..))
import Core.Tree.Instanciation (InstanciationF (..))
import Data.Vector.Strict (fromList)
import qualified Semantic.Index.Evidence as Evidence (Index (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Unify.Evidence (Evidence)

constrain ::
  (Monad m) =>
  m (Evidence s scope) ->
  (Type2.Index scope -> t -> m (Evidence s scope)) ->
  Type2.Index scope ->
  Type2.Index scope ->
  [t] ->
  m (Evidence s scope)
constrain fallthough constrain classx head = table classx head
  where
    table Type2.Num Type2.Integer [] =
      pure $ single
    table Type2.Num Type2.Int [] =
      pure $ single
    table Type2.Num Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call [integer]
    table Type2.Enum Type2.Bool [] =
      pure $ single
    table Type2.Enum Type2.Char [] =
      pure $ single
    table Type2.Enum Type2.Integer [] =
      pure $ single
    table Type2.Enum Type2.Int [] =
      pure $ single
    table Type2.Enum (Type2.Tuple 0) [] =
      pure $ single
    table Type2.Enum Type2.Ordering [] =
      pure $ single
    table Type2.Enum Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call [integer]
    table Type2.Eq Type2.Bool [] =
      pure $ single
    table Type2.Eq Type2.Char [] =
      pure $ single
    table Type2.Eq (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Eq) types
          pure $ call types
    table Type2.Eq Type2.Ordering [] =
      pure $ single
    table Type2.Eq Type2.Integer [] =
      pure $ single
    table Type2.Eq Type2.Int [] =
      pure $ single
    table Type2.Eq Type2.List [element] = do
      element <- constrain Type2.Eq element
      pure $ call [element]
    table Type2.Eq Type2.Ratio [integer] = do
      integer <- constrain Type2.Eq integer
      pure $ call [integer]
    table Type2.Ord Type2.Char [] =
      pure $ single
    table Type2.Ord (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Ord) types
          pure $ call types
    table Type2.Ord Type2.Int [] =
      pure $ single
    table Type2.Ord Type2.Integer [] =
      pure $ single
    table Type2.Ord Type2.Bool [] =
      pure $ single
    table Type2.Ord Type2.List [element] = do
      element <- constrain Type2.Ord element
      pure $ call [element]
    table Type2.Ord Type2.Ordering [] =
      pure $ single
    table Type2.Ord Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call [integer]
    table Type2.Real Type2.Int [] =
      pure $ single
    table Type2.Real Type2.Integer [] =
      pure $ single
    table Type2.Real Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call [integer]
    table Type2.Integral Type2.Int [] =
      pure $ single
    table Type2.Integral Type2.Integer [] =
      pure $ single
    table Type2.Fractional Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call [integer]
    table Type2.Functor Type2.List [] =
      pure $ single
    table Type2.Applicative Type2.List [] =
      pure $ single
    table Type2.Monad Type2.List [] =
      pure $ single
    table Type2.MonadFail Type2.List [] =
      pure $ single
    table Type2.Functor Type2.ST [_] =
      pure $ single
    table Type2.Applicative Type2.ST [_] =
      pure $ single
    table Type2.Monad Type2.ST [_] =
      pure $ single
    table _ _ _ = fallthough

    single = Variable (Evidence.Direct classx head) Mono
    call list = Variable (Evidence.Direct classx head) $ Instanciation $ fromList list
