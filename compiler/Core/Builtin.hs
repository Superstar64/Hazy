module Core.Builtin where

import {-# SOURCE #-} qualified Builtin.Applicative as Applicative
import {-# SOURCE #-} qualified Builtin.Bool as Bool
import {-# SOURCE #-} qualified Builtin.Enum as Enum
import {-# SOURCE #-} qualified Builtin.Eq as Eq
import {-# SOURCE #-} qualified Builtin.Fractional as Fractional
import {-# SOURCE #-} qualified Builtin.Functor as Functor
import {-# SOURCE #-} qualified Builtin.Integral as Integral
import {-# SOURCE #-} qualified Builtin.List as List
import {-# SOURCE #-} qualified Builtin.Monad as Monad
import {-# SOURCE #-} qualified Builtin.MonadFail as MonadFail
import {-# SOURCE #-} qualified Builtin.Num as Num
import {-# SOURCE #-} qualified Builtin.Ord as Ord
import {-# SOURCE #-} qualified Builtin.Ordering as Ordering
import {-# SOURCE #-} qualified Builtin.Ratio as Ratio
import {-# SOURCE #-} qualified Builtin.Real as Real
import qualified Builtin.Tuple as Tuple
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
    Type2.Bool -> pure Bool.definition
    Type2.List -> pure List.definition
    Type2.Tuple n -> pure (Tuple.definition n)
    Type2.Ordering -> pure Ordering.definition
    Type2.Ratio -> pure Ratio.definition
    _ -> error "bad data index"

instance Builtin Class where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Num -> pure Num.definition
    Type2.Enum -> pure Enum.definition
    Type2.Eq -> pure Eq.definition
    Type2.Ord -> pure Ord.definition
    Type2.Real -> pure Real.definition
    Type2.Integral -> pure Integral.definition
    Type2.Fractional -> pure Fractional.definition
    Type2.Functor -> pure Functor.definition
    Type2.Applicative -> pure Applicative.definition
    Type2.Monad -> pure Monad.definition
    Type2.MonadFail -> pure MonadFail.definition
    _ -> error "bad class index"

instance Builtin ClassExtra where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Num -> pure Num.extra
    Type2.Enum -> pure Enum.extra
    Type2.Eq -> pure Eq.extra
    Type2.Ord -> pure Ord.extra
    Type2.Real -> pure Real.extra
    Type2.Integral -> pure Integral.extra
    Type2.Fractional -> pure Fractional.extra
    Type2.Functor -> pure Functor.extra
    Type2.Applicative -> pure Applicative.extra
    Type2.Monad -> pure Monad.extra
    Type2.MonadFail -> pure MonadFail.extra
    _ -> error "bad class index"
