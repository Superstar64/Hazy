module Stage2.Index.Method where

import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Prelude hiding (Enum, Eq)
import qualified Prelude

data Index scope = Index
  {typeIndex :: !(Type2.Index scope), methodIndex :: !Int}
  deriving (Show, Prelude.Eq, Ord)

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category (Index typeIndex selectorIndex) = Index (Shift.map category typeIndex) selectorIndex

data Num
  = Plus
  | Minus
  | Multiply
  | Negate
  | Abs
  | Signum
  | FromInteger
  deriving (Prelude.Enum, Bounded)

plus = Index Type2.Num $ Prelude.fromEnum Plus

minus = Index Type2.Num $ Prelude.fromEnum Minus

multiply = Index Type2.Num $ Prelude.fromEnum Multiply

negate = Index Type2.Num $ Prelude.fromEnum Negate

abs = Index Type2.Num $ Prelude.fromEnum Abs

signum = Index Type2.Num $ Prelude.fromEnum Signum

fromInteger = Index Type2.Num $ Prelude.fromEnum FromInteger

data Enum
  = Succ
  | Pred
  | ToEnum
  | FromEnum
  | EnumFrom
  | EnumFromThen
  | EnumFromTo
  | EnumFromThenTo
  deriving (Prelude.Enum, Bounded)

succ = Index Type2.Enum $ Prelude.fromEnum Succ

pred = Index Type2.Enum $ Prelude.fromEnum Pred

toEnum = Index Type2.Enum $ Prelude.fromEnum ToEnum

fromEnum = Index Type2.Enum $ Prelude.fromEnum FromEnum

enumFrom = Index Type2.Enum $ Prelude.fromEnum EnumFrom

enumFromThen = Index Type2.Enum $ Prelude.fromEnum EnumFromThen

enumFromTo = Index Type2.Enum $ Prelude.fromEnum EnumFromTo

enumFromThenTo = Index Type2.Enum $ Prelude.fromEnum EnumFromThenTo

data Eq
  = Equal
  | NotEqual
  deriving (Prelude.Enum, Bounded)

equal = Index Type2.Eq $ Prelude.fromEnum Equal

notEqual = Index Type2.Eq $ Prelude.fromEnum NotEqual
