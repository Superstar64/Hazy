module Semantic.Index.Method where

import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (Bounded, Enum, Eq, Ord, Show)
import qualified Prelude

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    methodIndex :: !Int
  }
  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

instance Shift0.Functor Index where
  map = Shift.mapDefault

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
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

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
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

succ = Index Type2.Enum $ Prelude.fromEnum Succ

pred = Index Type2.Enum $ Prelude.fromEnum Pred

toEnum = Index Type2.Enum $ Prelude.fromEnum ToEnum

fromEnum = Index Type2.Enum $ Prelude.fromEnum FromEnum

enumFrom = Index Type2.Enum $ Prelude.fromEnum EnumFrom

enumFromThen = Index Type2.Enum $ Prelude.fromEnum EnumFromThen

enumFromTo = Index Type2.Enum $ Prelude.fromEnum EnumFromTo

enumFromThenTo = Index Type2.Enum $ Prelude.fromEnum EnumFromThenTo

data Bounded
  = MinBound
  | MaxBound
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

minBound = Index Type2.Bounded $ Prelude.fromEnum MinBound

maxBound = Index Type2.Bounded $ Prelude.fromEnum MaxBound

data Eq
  = Equal
  | NotEqual
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

equal = Index Type2.Eq $ Prelude.fromEnum Equal

notEqual = Index Type2.Eq $ Prelude.fromEnum NotEqual

data Ord
  = Compare
  | LessThen
  | LessThenEqual
  | GreaterThen
  | GreaterThenEqual
  | Max
  | Min
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

compare = Index Type2.Ord $ Prelude.fromEnum Compare

lessThen = Index Type2.Ord $ Prelude.fromEnum LessThen

lessThenEqual = Index Type2.Ord $ Prelude.fromEnum LessThenEqual

greaterThen = Index Type2.Ord $ Prelude.fromEnum GreaterThen

greaterThenEqual = Index Type2.Ord $ Prelude.fromEnum GreaterThenEqual

max = Index Type2.Ord $ Prelude.fromEnum Max

min = Index Type2.Ord $ Prelude.fromEnum Min

data Real = ToRational
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

toRational = Index Type2.Real $ Prelude.fromEnum ToRational

data Integral = Quot | Rem | Div | Mod | QuotRem | DivMod | ToInteger
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

quot = Index Type2.Integral $ Prelude.fromEnum Quot

rem = Index Type2.Integral $ Prelude.fromEnum Rem

div = Index Type2.Integral $ Prelude.fromEnum Div

mod = Index Type2.Integral $ Prelude.fromEnum Mod

quotRem = Index Type2.Integral $ Prelude.fromEnum QuotRem

divMod = Index Type2.Integral $ Prelude.fromEnum DivMod

toInteger = Index Type2.Integral $ Prelude.fromEnum ToInteger

data Fractional = Divide | Recip | FromRational
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

divide = Index Type2.Fractional $ Prelude.fromEnum Divide

recip = Index Type2.Fractional $ Prelude.fromEnum Recip

fromRational = Index Type2.Fractional $ Prelude.fromEnum FromRational

data Functor
  = Fmap
  | Fconst
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

fmap = Index Type2.Functor $ Prelude.fromEnum Fmap

fconst = Index Type2.Functor $ Prelude.fromEnum Fconst

data Applicative
  = Pure
  | Ap
  | LiftA2
  | DiscardLeft
  | DiscardRight
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

pure = Index Type2.Applicative $ Prelude.fromEnum Pure

ap = Index Type2.Applicative $ Prelude.fromEnum Ap

liftA2 = Index Type2.Applicative $ Prelude.fromEnum LiftA2

discardLeft = Index Type2.Applicative $ Prelude.fromEnum DiscardLeft

discardRight = Index Type2.Applicative $ Prelude.fromEnum DiscardRight

data Monad
  = Bind
  | Then
  | Return
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

bind = Index Type2.Monad $ Prelude.fromEnum Bind

thenx = Index Type2.Monad $ Prelude.fromEnum Then

return = Index Type2.Monad $ Prelude.fromEnum Return

data MonadFail
  = Fail
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

fail = Index Type2.MonadFail $ Prelude.fromEnum Fail

data Semigroup
  = Combine
  | Sconcat
  | Stimes
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

combine = Index Type2.Semigroup $ Prelude.fromEnum Combine

sconcat = Index Type2.Semigroup $ Prelude.fromEnum Sconcat

stimes = Index Type2.Semigroup $ Prelude.fromEnum Stimes

data Monoid
  = Mempty
  | Mappend
  | Mconcat
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

mempty = Index Type2.Monoid $ Prelude.fromEnum Mempty

mappend = Index Type2.Monoid $ Prelude.fromEnum Mappend

mconcat = Index Type2.Monoid $ Prelude.fromEnum Mconcat

data Show = ShowsPrec | Show | ShowList
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

showsPrec = Index Type2.Show $ Prelude.fromEnum ShowsPrec

show = Index Type2.Show $ Prelude.fromEnum Show

showList = Index Type2.Show $ Prelude.fromEnum ShowList

data Read = ReadsPrec | ReadList
  deriving (Prelude.Enum, Prelude.Bounded, Prelude.Show)

readsPrec = Index Type2.Read $ Prelude.fromEnum ReadsPrec

readList = Index Type2.Read $ Prelude.fromEnum ReadList
