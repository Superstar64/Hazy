module Stage4.Tree.Builtin.Enum where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import Stage4.Tree.Class (Class (Class))
import qualified Stage4.Tree.Class as Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Stage4.Tree.Expression (Expression (Hook, hook))
import Stage4.Tree.Hook (Hook (DefaultEnum))
import qualified Stage4.Tree.Hook
import qualified Stage4.Tree.Instanciation as Instanciation
import qualified Stage4.Tree.Scheme as Scheme
import qualified Stage4.Tree.Type as Type (Type (..), smallType)

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
        go enum =
          Hook {hook = DefaultEnum {enum, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.empty
