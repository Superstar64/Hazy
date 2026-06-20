module Core.Tree.Builtin.Num where

import Core.Tree.Class (Class (Class))
import qualified Core.Tree.Class as Class
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (DefaultNum))
import qualified Core.Tree.Hook
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Scheme as Scheme
import qualified Core.Tree.Type as Type (Type (..), smallType)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2

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
        go num =
          Hook {hook = DefaultNum {num, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
