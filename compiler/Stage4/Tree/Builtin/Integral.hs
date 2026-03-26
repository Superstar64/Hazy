module Stage4.Tree.Builtin.Integral where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import Stage4.Tree.Class (Class (Class))
import qualified Stage4.Tree.Class as Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Constraint (Constraint (..))
import Stage4.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Stage4.Tree.Expression (Expression (Hook, hook))
import Stage4.Tree.Hook (Hook (..))
import qualified Stage4.Tree.Instanciation as Instanciation
import qualified Stage4.Tree.Scheme as Scheme
import qualified Stage4.Tree.Type as Type (Type (..), smallType)

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
            instanciation = Instanciation.empty
