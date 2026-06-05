module Semantic.Index.Evidence where

import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift

data Index scope
  = Index !(Evidence0.Index scope)
  | Class !(Type.Index scope) !(Type2.Index scope)
  | Data !(Type2.Index scope) (Type.Index scope)
  | Builtin !Builtin
  deriving (Eq, Show)

data Builtin
  = NumInteger
  | NumInt
  | NumRatio
  | EnumBool
  | EnumChar
  | EnumInteger
  | EnumInt
  | EnumOrdering
  | EnumUnit
  | EnumRatio
  | EqBool
  | EqChar
  | EqTuple !Int
  | EqInteger
  | EqInt
  | EqList
  | EqOrdering
  | EqRatio
  | OrdChar
  | OrdTuple !Int
  | OrdInt
  | OrdInteger
  | OrdBool
  | OrdList
  | OrdOrdering
  | OrdRatio
  | RealInt
  | RealInteger
  | RealRatio
  | IntegralInt
  | IntegralInteger
  | FractionalRatio
  | FunctorList
  | ApplicativeList
  | MonadList
  | MonadFailList
  | FunctorST
  | ApplicativeST
  | MonadST
  deriving (Eq, Show)

assumed :: Int -> Index (Scope.Local ':+ scopes)
assumed = Index . Evidence0.Assumed

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category = \case
    Index index -> Index (Shift.map category index)
    Class classx head -> Class (Shift.map category classx) (Shift.map category head)
    Data classx head -> Data (Shift.map category classx) (Shift.map category head)
    Builtin builtin -> Builtin builtin

instance Shift.PartialUnshift Index where
  partialUnshift fail = \case
    Index index -> Index <$> Shift.partialUnshift fail index
    Class classx head ->
      Class
        <$> Shift.partialUnshift fail classx
        <*> Shift.partialUnshift fail head
    Data classx head ->
      Data
        <$> Shift.partialUnshift fail classx
        <*> Shift.partialUnshift fail head
    Builtin builtin -> pure $ Builtin builtin
