module Stage4.Tree.Builtin where

import Data.Foldable (Foldable (toList))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type as Type (Index)
import qualified Stage2.Index.Type2 as Type2
import Stage4.Tree.Class (Class (Class))
import qualified Stage4.Tree.Class as Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Constructor (Constructor (..))
import Stage4.Tree.Data (Data (Data))
import qualified Stage4.Tree.Data as Data
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import qualified Stage4.Tree.Scheme as Scheme
import qualified Stage4.Tree.Statements as Statements
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

numExtra :: ClassExtra scope
numExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        -- todo proper defaults for num
        go :: Method.Num -> Expression scope
        go _ = Expression.Join {statements = Statements.Bottom}

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

enumExtra :: ClassExtra scope
enumExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        -- todo proper defaults for enum
        go :: Method.Enum -> Expression scope
        go _ = Expression.Join {statements = Statements.Bottom}

eq :: Class scope
eq =
  Class
    { parameter = Type.smallType,
      constraints = Strict.Vector.empty,
      methods = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        var = Type.Variable (Local.Local 0)
        go Method.Equal = Scheme.mono $ var `Type.Function` var `Type.Function` Type.Constructor Type2.Bool
        go Method.NotEqual = Scheme.mono $ var `Type.Function` var `Type.Function` Type.Constructor Type2.Bool

eqExtra :: ClassExtra scope
eqExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        -- todo proper defaults for num
        go :: Method.Eq -> Expression scope
        go _ = Expression.Join {statements = Statements.Bottom}
