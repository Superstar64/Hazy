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
    table Type2.Num Type2.Integer [] = single
    table Type2.Num Type2.Int [] = single
    table Type2.Num Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      call [integer]
    table Type2.Enum Type2.Bool [] = single
    table Type2.Enum Type2.Char [] = single
    table Type2.Enum Type2.Integer [] = single
    table Type2.Enum Type2.Int [] = single
    table Type2.Enum (Type2.Tuple 0) [] = single
    table Type2.Enum Type2.Ordering [] = single
    table Type2.Enum Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      call [integer]
    table Type2.Bounded Type2.Bool [] = single
    table Type2.Bounded Type2.Char [] = single
    table Type2.Bounded Type2.Int [] = single
    table Type2.Bounded Type2.Ordering [] = single
    table Type2.Bounded (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Bounded) types
          call types
    table Type2.Eq Type2.Bool [] = single
    table Type2.Eq Type2.Char [] = single
    table Type2.Eq (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Eq) types
          call types
    table Type2.Eq Type2.Ordering [] = single
    table Type2.Eq Type2.Integer [] = single
    table Type2.Eq Type2.Int [] = single
    table Type2.Eq Type2.List [element] = do
      element <- constrain Type2.Eq element
      call [element]
    table Type2.Eq Type2.NonEmpty [element] = do
      element <- constrain Type2.Eq element
      call [element]
    table Type2.Eq Type2.Ratio [integer] = do
      integer <- constrain Type2.Eq integer
      call [integer]
    table Type2.Ord Type2.Char [] = single
    table Type2.Ord (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Ord) types
          call types
    table Type2.Ord Type2.Int [] = single
    table Type2.Ord Type2.Integer [] = single
    table Type2.Ord Type2.Bool [] = single
    table Type2.Ord Type2.List [element] = do
      element <- constrain Type2.Ord element
      call [element]
    table Type2.Ord Type2.NonEmpty [element] = do
      element <- constrain Type2.Ord element
      call [element]
    table Type2.Ord Type2.Ordering [] = single
    table Type2.Ord Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      call [integer]
    table Type2.Real Type2.Int [] = single
    table Type2.Real Type2.Integer [] = single
    table Type2.Real Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      call [integer]
    table Type2.Integral Type2.Int [] = single
    table Type2.Integral Type2.Integer [] = single
    table Type2.Fractional Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      call [integer]
    table Type2.Functor Type2.List [] = single
    table Type2.Functor Type2.NonEmpty [] = single
    table Type2.Applicative Type2.List [] = single
    table Type2.Applicative Type2.NonEmpty [] = single
    table Type2.Monad Type2.List [] = single
    table Type2.Monad Type2.NonEmpty [] = single
    table Type2.MonadFail Type2.List [] = single
    table Type2.Functor Type2.ST [_] = single
    table Type2.Applicative Type2.ST [_] = single
    table Type2.Monad Type2.ST [_] = single
    table Type2.Semigroup Type2.Arrow [_, result] = do
      result <- constrain Type2.Semigroup result
      call [result]
    table Type2.Semigroup Type2.List [_] = single
    table Type2.Semigroup Type2.NonEmpty [_] = single
    table Type2.Semigroup Type2.Ordering [] = single
    table Type2.Semigroup Type2.ST [_, result] = do
      result <- constrain Type2.Semigroup result
      call [result]
    table Type2.Semigroup (Type2.Tuple n) elements
      | n == length elements = do
          elements <- traverse (constrain Type2.Semigroup) elements
          call elements
    table Type2.Monoid Type2.Arrow [_, result] = do
      result <- constrain Type2.Monoid result
      call [result]
    table Type2.Monoid Type2.List [_] = single
    table Type2.Monoid Type2.Ordering [] = single
    table Type2.Monoid Type2.ST [_, result] = do
      result <- constrain Type2.Monoid result
      call [result]
    table Type2.Monoid (Type2.Tuple n) elements
      | n == length elements = do
          elements <- traverse (constrain Type2.Monoid) elements
          call elements
    table _ _ _ = fallthough

    single = pure $ Variable (Evidence.Direct classx head) Mono
    call list = pure $ Variable (Evidence.Direct classx head) $ Instanciation $ fromList list
