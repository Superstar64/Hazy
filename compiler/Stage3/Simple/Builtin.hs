module Stage3.Simple.Builtin where

import Data.Foldable (Foldable (toList))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type as Type (Index)
import qualified Stage2.Index.Type2 as Type2
import Stage3.Simple.Class (Class (Class))
import qualified Stage3.Simple.Class as Class
import Stage3.Simple.Constructor (Constructor (..))
import Stage3.Simple.Data (Data (Data))
import qualified Stage3.Simple.Data as Data
import qualified Stage3.Simple.Scheme as Scheme
import Stage3.Simple.Type (Type)
import qualified Stage3.Simple.Type as Type (Type (..), smallType)

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
    _ -> error "bad class index"

bool :: Data scope
bool =
  Data
    { parameters = Strict.Vector.empty,
      constructors = Strict.Vector.fromList set,
      selectors = Strict.Vector.empty
    }
  where
    set = map go [minBound .. maxBound]
      where
        go Constructor.False = Constructor {entries = Strict.Vector.empty}
        go Constructor.True = Constructor {entries = Strict.Vector.empty}

list :: Data scope
list =
  Data
    { parameters = Strict.Vector.singleton Type.smallType,
      constructors = Strict.Vector.fromList $ toList set,
      selectors = Strict.Vector.empty
    }
  where
    set = map go [minBound .. maxBound]
      where
        go Constructor.Nil = Constructor {entries = Strict.Vector.empty}
        go Constructor.Cons = Constructor {entries = Strict.Vector.fromList [head, tail]}
          where
            head = Type.Variable $ Local.Local 0
            tail = Type.Constructor Type2.List `Type.Call` Type.Variable (Local.Local 0)

tuple :: Int -> Data scope
tuple n =
  Data
    { parameters = Strict.Vector.replicate n Type.smallType,
      constructors = Strict.Vector.fromList $ toList set,
      selectors = Strict.Vector.empty
    }
  where
    set = map go [minBound .. maxBound]
    go Constructor.Tuple =
      Constructor
        { entries = Strict.Vector.generate n $ Type.Variable . Local.Local
        }

num :: Class scope
num =
  Class
    { parameter = Type.smallType,
      constraints = Strict.Vector.empty,
      methods = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        var = Type.Variable (Local.Local 0)
        go Method.Plus = Scheme.mono $ var `Type.Function` var `Type.Function` var
        go Method.Minus = Scheme.mono $ var `Type.Function` var `Type.Function` var
        go Method.Multiply = Scheme.mono $ var `Type.Function` var `Type.Function` var
        go Method.Negate = Scheme.mono $ var `Type.Function` var
        go Method.Abs = Scheme.mono $ var `Type.Function` var
        go Method.Signum = Scheme.mono $ var `Type.Function` var
        go Method.FromInteger = Scheme.mono $ Type.Constructor Type2.Integer `Type.Function` var

enum :: Class scope
enum =
  Class
    { parameter = Type.smallType,
      constraints = Strict.Vector.empty,
      methods = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        var = Type.Variable (Local.Local 0)
        go Method.Succ = Scheme.mono $ var `Type.Function` var
        go Method.Pred = Scheme.mono $ var `Type.Function` var
        go Method.ToEnum = Scheme.mono $ Type.Constructor Type2.Int `Type.Function` var
        go Method.FromEnum = Scheme.mono $ var `Type.Function` Type.Constructor Type2.Int
        go Method.EnumFrom =
          Scheme.mono $ var `Type.Function` Type.Constructor Type2.List `Type.Call` var
        go Method.EnumFromThen =
          Scheme.mono $ var `Type.Function` var `Type.Function` Type.Constructor Type2.List `Type.Call` var
        go Method.EnumFromTo =
          Scheme.mono $ var `Type.Function` var `Type.Function` Type.Constructor Type2.List `Type.Call` var
        go Method.EnumFromThenTo =
          Scheme.mono $
            var `Type.Function` var `Type.Function` var `Type.Function` Type.Constructor Type2.List `Type.Call` var
