module Stage4.Tree.Builtin where

import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Type as Type (Index)
import qualified Stage2.Index.Type2 as Type2
import Stage4.Tree.Builtin.Bool (bool)
import Stage4.Tree.Builtin.Enum (enum, enumExtra)
import Stage4.Tree.Builtin.Eq (eq, eqExtra)
import Stage4.Tree.Builtin.List (list)
import Stage4.Tree.Builtin.Num (num, numExtra)
import Stage4.Tree.Builtin.Tuple (tuple)
import Stage4.Tree.Class (Class)
import qualified Stage4.Tree.Class as Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Data (Data)
import qualified Stage4.Tree.Data as Data
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type (Type (..), smallType)

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
    Type2.Char -> Type.smallType
    Type2.ST -> Type.smallType `Type.Function` Type.smallType `Type.Function` Type.smallType
    Type2.Arrow -> Type.smallType `Type.Function` Type.smallType `Type.Function` Type.smallType
    Type2.Integer -> Type.smallType
    Type2.Int -> Type.smallType
    Type2.Num -> classKind
    Type2.Enum -> classKind
    Type2.Eq -> classKind
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
    _ -> error "bad data index"

instance Builtin Class where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Num -> pure num
    Type2.Enum -> pure enum
    Type2.Eq -> pure eq
    _ -> error "bad class index"

instance Builtin ClassExtra where
  index pure normal = \case
    Type2.Index index -> normal index
    Type2.Num -> pure numExtra
    Type2.Enum -> pure enumExtra
    Type2.Eq -> pure eqExtra
    _ -> error "bad class index"
