module Semantic.Index.Type2 where

import {-# SOURCE #-} qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type as Type1
import Semantic.Scope (Environment ((:+)), Local)
import Prelude hiding (map, traverse)

data Index scope
  = Index !(Type1.Index scope)
  | Lifted !(Constructor.Index scope)
  | Bool
  | Char
  | ST
  | Arrow
  | List
  | NonEmpty
  | Tuple !Int
  | Integer
  | Int
  | Ordering
  | Ratio
  | Num
  | Enum
  | Bounded
  | Eq
  | Ord
  | Real
  | Integral
  | Fractional
  | Functor
  | Applicative
  | Monad
  | MonadFail
  | Semigroup
  | Monoid
  | Lazy
  | Strict
  deriving (Show, Eq, Ord)

data Split scope
  = Normal !(Type1.Index scope)
  | Constructor !(Constructor.Index scope)
  | Builtin !(forall scope. Index scope)

split :: Index scope -> Split scope
split = \case
  Index index -> Normal index
  Lifted constructor -> Constructor constructor
  Bool -> Builtin Bool
  Char -> Builtin Char
  ST -> Builtin ST
  Arrow -> Builtin Arrow
  List -> Builtin List
  NonEmpty -> Builtin NonEmpty
  Tuple count -> Builtin (Tuple count)
  Integer -> Builtin Integer
  Int -> Builtin Int
  Ordering -> Builtin Ordering
  Ratio -> Builtin Ratio
  Num -> Builtin Num
  Enum -> Builtin Enum
  Bounded -> Builtin Bounded
  Eq -> Builtin Eq
  Ord -> Builtin Ord
  Real -> Builtin Real
  Integral -> Builtin Integral
  Fractional -> Builtin Fractional
  Functor -> Builtin Functor
  Applicative -> Builtin Applicative
  Monad -> Builtin Monad
  MonadFail -> Builtin MonadFail
  Semigroup -> Builtin Semigroup
  Monoid -> Builtin Monoid
  Lazy -> Builtin Lazy
  Strict -> Builtin Strict

unlocal :: Index (Local ':+ scope) -> Index scope
unlocal index = case split index of
  Normal normal -> Index $ Type1.unlocal normal
  Constructor lifted -> Lifted $ Constructor.unlocal lifted
  Builtin builtin -> builtin
