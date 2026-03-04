module Stage2.Index.Method where

import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Prelude hiding (Enum, Eq)
import qualified Prelude

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    methodIndex :: !Int
  }
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
  deriving (Prelude.Enum, Bounded, Show)

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
  deriving (Prelude.Enum, Bounded, Show)

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
  deriving (Prelude.Enum, Bounded, Show)

equal = Index Type2.Eq $ Prelude.fromEnum Equal

notEqual = Index Type2.Eq $ Prelude.fromEnum NotEqual

data Functor
  = Fmap
  | Fconst
  deriving (Prelude.Enum, Bounded, Show)

fmap = Index Type2.Functor $ Prelude.fromEnum Fmap

fconst = Index Type2.Functor $ Prelude.fromEnum Fconst

data Applicative
  = Pure
  | Ap
  | LiftA2
  | DiscardLeft
  | DiscardRight
  deriving (Prelude.Enum, Bounded, Show)

pure = Index Type2.Applicative $ Prelude.fromEnum Pure

ap = Index Type2.Applicative $ Prelude.fromEnum Ap

liftA2 = Index Type2.Applicative $ Prelude.fromEnum LiftA2

discardLeft = Index Type2.Applicative $ Prelude.fromEnum DiscardLeft

discardRight = Index Type2.Applicative $ Prelude.fromEnum DiscardRight

data Monad
  = Bind
  | Then
  | Return
  deriving (Prelude.Enum, Bounded, Show)

bind = Index Type2.Monad $ Prelude.fromEnum Bind

thenx = Index Type2.Monad $ Prelude.fromEnum Then

return = Index Type2.Monad $ Prelude.fromEnum Return

data MonadFail
  = Fail
  deriving (Prelude.Enum, Bounded, Show)

fail = Index Type2.MonadFail $ Prelude.fromEnum Fail
