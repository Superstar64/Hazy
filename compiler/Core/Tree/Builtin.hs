module Core.Tree.Builtin where

import Core.Tree.Builtin.Applicative (applicative, applicativeExtra)
import Core.Tree.Builtin.Bool (bool)
import Core.Tree.Builtin.Enum (enum, enumExtra)
import Core.Tree.Builtin.Eq (eq, eqExtra)
import Core.Tree.Builtin.Fractional (fractional, fractionalExtra)
import Core.Tree.Builtin.Functor (functor, functorExtra)
import Core.Tree.Builtin.Integral (integral, integralExtra)
import Core.Tree.Builtin.List (list)
import Core.Tree.Builtin.Monad (monad, monadExtra)
import Core.Tree.Builtin.MonadFail (monadFail, monadFailExtra)
import Core.Tree.Builtin.Num (num, numExtra)
import Core.Tree.Builtin.Ord (ord, ordExtra)
import Core.Tree.Builtin.Ordering (ordering)
import Core.Tree.Builtin.Ratio (ratio)
import Core.Tree.Builtin.Real (real, realExtra)
import Core.Tree.Builtin.Tuple (tuple)
import Core.Tree.Class (Class)
import qualified Core.Tree.Class as Class
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Data (Data)
import qualified Core.Tree.Data as Data
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type (Type (..), smallType)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type as Type (Index)
import qualified Semantic.Index.Type2 as Type2

kind ::
  (Type scope -> typex) ->
  (Type.Index scope -> typex) ->
  (Constructor.Index scope -> typex) ->
  Type2.Index scope ->
  typex
kind pure typex constructor = \case
  Type2.Index normal -> typex normal
  Type2.Lifted normal -> constructor normal
  real -> pure $ case real of
    Type2.Bool -> dataKind
    Type2.List -> dataKind
    Type2.Tuple {} -> dataKind
    Type2.Ordering -> dataKind
    Type2.Ratio -> dataKind
    Type2.Char -> Type.smallType
    Type2.ST -> Type.smallType `Type.Function` Type.smallType `Type.Function` Type.smallType
    Type2.Arrow -> Type.smallType `Type.Function` Type.smallType `Type.Function` Type.smallType
    Type2.Integer -> Type.smallType
    Type2.Int -> Type.smallType
    Type2.Num -> classKind
    Type2.Enum -> classKind
    Type2.Eq -> classKind
    Type2.Ord -> classKind
    Type2.Real -> classKind
    Type2.Integral -> classKind
    Type2.Fractional -> classKind
    Type2.Functor -> classKind
    Type2.Applicative -> classKind
    Type2.Monad -> classKind
    Type2.MonadFail -> classKind
    Type2.Lazy -> Type.Levity
    Type2.Strict -> Type.Levity
    where
      dataKind = Data.kind $ index id (error "bad index") real
      classKind = Class.kind $ index id (error "bad index") real

class Builtin builtin where
  index ::
    (builtin scope -> target) ->
    (Type.Index scope -> target) ->
    Type2.Index scope ->
    target

instance Builtin Data where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Bool -> pure bool
    Type2.List -> pure list
    Type2.Tuple n -> pure (tuple n)
    Type2.Ordering -> pure ordering
    Type2.Ratio -> pure ratio
    _ -> error "bad data index"

instance Builtin Class where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Num -> pure num
    Type2.Enum -> pure enum
    Type2.Eq -> pure eq
    Type2.Ord -> pure ord
    Type2.Real -> pure real
    Type2.Integral -> pure integral
    Type2.Fractional -> pure fractional
    Type2.Functor -> pure functor
    Type2.Applicative -> pure applicative
    Type2.Monad -> pure monad
    Type2.MonadFail -> pure monadFail
    _ -> error "bad class index"

instance Builtin ClassExtra where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Num -> pure numExtra
    Type2.Enum -> pure enumExtra
    Type2.Eq -> pure eqExtra
    Type2.Ord -> pure ordExtra
    Type2.Real -> pure realExtra
    Type2.Integral -> pure integralExtra
    Type2.Fractional -> pure fractionalExtra
    Type2.Functor -> pure functorExtra
    Type2.Applicative -> pure applicativeExtra
    Type2.Monad -> pure monadExtra
    Type2.MonadFail -> pure monadFailExtra
    _ -> error "bad class index"
