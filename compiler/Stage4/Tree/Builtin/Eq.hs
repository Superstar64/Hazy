module Stage4.Tree.Builtin.Eq where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import Stage4.Tree.Class (Class (Class))
import qualified Stage4.Tree.Class as Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Evidence (Evidence (Variable, variable))
import Stage4.Tree.Expression (Expression (Hook, hook))
import Stage4.Tree.Hook (Hook (DefaultEq))
import qualified Stage4.Tree.Hook
import qualified Stage4.Tree.Scheme as Scheme
import qualified Stage4.Tree.Type as Type (Type (..), smallType)

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
        go eq =
          Hook {hook = DefaultEq {eq, evidence = Variable {variable}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
