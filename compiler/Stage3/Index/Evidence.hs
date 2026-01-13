module Stage3.Index.Evidence where

import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence0 as Evidence0

data Index scope
  = Index !(Evidence0.Index scope)
  | Class !(Type.Index scope) !(Type2.Index scope)
  | Data !(Type2.Index scope) (Type.Index scope)
  | NumInteger
  | NumInt
  | EnumInteger
  | EnumInt
  | EqBool
  | EqChar
  | EqInteger
  | EqInt
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
    NumInteger -> NumInteger
    NumInt -> NumInt
    EnumInteger -> EnumInteger
    EnumInt -> EnumInt
    EqBool -> EqBool
    EqChar -> EqChar
    EqInt -> EqInt
    EqInteger -> EqInteger

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
    e -> pure $ case e of
      NumInteger -> NumInteger
      NumInt -> NumInt
      EnumInteger -> EnumInteger
      EnumInt -> EnumInt
      EqBool -> EqBool
      EqChar -> EqChar
      EqInt -> EqInt
      EqInteger -> EqInteger
