module Core.Tree.Builtin.Integral where

import Core.Tree.Class (Class (Class))
import qualified Core.Tree.Class as Class
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Constraint (Constraint (..))
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (..))
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Scheme as Scheme
import qualified Core.Tree.Type as Type (Type (..), smallType)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2

integral :: Class scope
integral =
  Class
    { parameter = Type.smallType,
      constraints =
        Strict.Vector.fromList
          [ Constraint
              { classx = Type2.Real,
                head = 0,
                arguments = Strict.Vector.empty
              },
            Constraint
              { classx = Type2.Enum,
                head = 0,
                arguments = Strict.Vector.empty
              }
          ],
      methods = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        a = Type.Variable (Local.Local 0)
        go Method.Quot = Scheme.mono $ a `Type.Function` a `Type.Function` a
        go Method.Rem = Scheme.mono $ a `Type.Function` a `Type.Function` a
        go Method.Div = Scheme.mono $ a `Type.Function` a `Type.Function` a
        go Method.Mod = Scheme.mono $ a `Type.Function` a `Type.Function` a
        go Method.QuotRem =
          Scheme.mono $
            a
              `Type.Function` a
              `Type.Function` Type.Constructor (Type2.Tuple 2)
              `Type.Call` a
              `Type.Call` a
        go Method.DivMod =
          Scheme.mono $
            a
              `Type.Function` a
              `Type.Function` Type.Constructor (Type2.Tuple 2)
              `Type.Call` a
              `Type.Call` a
        go Method.ToInteger =
          Scheme.mono $
            a `Type.Function` Type.Constructor Type2.Integer

integralExtra :: ClassExtra scope
integralExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go integral =
          Hook {hook = DefaultIntegral {integral, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
