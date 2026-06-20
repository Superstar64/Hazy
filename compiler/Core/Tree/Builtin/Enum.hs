module Core.Tree.Builtin.Enum where

import Core.Tree.Class (Class (Class))
import qualified Core.Tree.Class as Class
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (DefaultEnum))
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
            instanciation = Instanciation.Mono
