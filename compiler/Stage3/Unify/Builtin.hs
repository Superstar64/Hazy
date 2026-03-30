module Stage3.Unify.Builtin where

import Data.Vector.Strict (fromList)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Index.Evidence as Evidence (Builtin (..), Index (..))
import Stage3.Unify.Evidence (Evidence)
import qualified Stage3.Unify.Evidence as Evidence (Evidence (..))
import Stage3.Unify.Instanciation (Instanciation (..))

constrain ::
  (Monad m) =>
  m (Evidence s scope) ->
  (Type2.Index scope -> t -> m (Evidence s scope)) ->
  Type2.Index scope ->
  Type2.Index scope ->
  [t] ->
  m (Evidence s scope)
constrain fallthough constrain = table
  where
    table Type2.Num Type2.Integer [] =
      pure $ single Evidence.NumInteger
    table Type2.Num Type2.Int [] =
      pure $ single Evidence.NumInt
    table Type2.Num Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call Evidence.NumRatio [integer]
    table Type2.Enum Type2.Bool [] =
      pure $ single Evidence.EnumBool
    table Type2.Enum Type2.Char [] =
      pure $ single Evidence.EnumChar
    table Type2.Enum Type2.Integer [] =
      pure $ single Evidence.EnumInteger
    table Type2.Enum Type2.Int [] =
      pure $ single Evidence.EnumInt
    table Type2.Enum (Type2.Tuple 0) [] =
      pure $ single Evidence.EnumUnit
    table Type2.Enum Type2.Ordering [] =
      pure $ single Evidence.EnumOrdering
    table Type2.Enum Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call Evidence.EnumRatio [integer]
    table Type2.Eq Type2.Bool [] =
      pure $ single Evidence.EqBool
    table Type2.Eq Type2.Char [] =
      pure $ single Evidence.EqChar
    table Type2.Eq (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Eq) types
          pure $ call (Evidence.EqTuple n) types
    table Type2.Eq Type2.Ordering [] =
      pure $ single Evidence.EqOrdering
    table Type2.Eq Type2.Integer [] =
      pure $ single Evidence.EqInteger
    table Type2.Eq Type2.Int [] =
      pure $ single Evidence.EqInt
    table Type2.Eq Type2.List [element] = do
      element <- constrain Type2.Eq element
      pure $ call Evidence.EqList [element]
    table Type2.Eq Type2.Ratio [integer] = do
      integer <- constrain Type2.Eq integer
      pure $ call Evidence.EqRatio [integer]
    table Type2.Ord Type2.Char [] =
      pure $ single Evidence.OrdChar
    table Type2.Ord (Type2.Tuple n) types
      | n == length types = do
          types <- traverse (constrain Type2.Ord) types
          pure $ call (Evidence.OrdTuple n) types
    table Type2.Ord Type2.Int [] =
      pure $ single Evidence.OrdInt
    table Type2.Ord Type2.Integer [] =
      pure $ single Evidence.OrdInteger
    table Type2.Ord Type2.Bool [] =
      pure $ single Evidence.OrdBool
    table Type2.Ord Type2.List [element] = do
      element <- constrain Type2.Ord element
      pure $ call Evidence.OrdList [element]
    table Type2.Ord Type2.Ordering [] =
      pure $ single Evidence.OrdOrdering
    table Type2.Ord Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call Evidence.OrdRatio [integer]
    table Type2.Real Type2.Int [] =
      pure $ single Evidence.RealInt
    table Type2.Real Type2.Integer [] =
      pure $ single Evidence.RealInteger
    table Type2.Real Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call Evidence.RealRatio [integer]
    table Type2.Integral Type2.Int [] =
      pure $ single Evidence.IntegralInt
    table Type2.Integral Type2.Integer [] =
      pure $ single Evidence.IntegralInteger
    table Type2.Fractional Type2.Ratio [integer] = do
      integer <- constrain Type2.Integral integer
      pure $ call Evidence.FractionalRatio [integer]
    table Type2.Functor Type2.List [] =
      pure $ single Evidence.FunctorList
    table Type2.Applicative Type2.List [] =
      pure $ single Evidence.ApplicativeList
    table Type2.Monad Type2.List [] =
      pure $ single Evidence.MonadList
    table Type2.MonadFail Type2.List [] =
      pure $ single Evidence.MonadFailList
    table Type2.Functor Type2.ST [_] =
      pure $ single Evidence.FunctorST
    table Type2.Applicative Type2.ST [_] =
      pure $ single Evidence.ApplicativeST
    table Type2.Monad Type2.ST [_] =
      pure $ single Evidence.MonadST
    table _ _ _ = fallthough

    single builtin = call builtin []
    call builtin list = Evidence.Variable (Evidence.Builtin builtin) $ Instanciation $ fromList list
