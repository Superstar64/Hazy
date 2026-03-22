module Stage4.Tree.Builtin.Ord where

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

ord :: Class scope
ord =
  Class
    { parameter = Type.smallType,
      constraints =
        Strict.Vector.singleton
          Constraint
            { classx = Type2.Eq,
              head = 0,
              arguments = Strict.Vector.empty
            },
      methods = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        a = Type.Variable (Local.Local 0)
        go Method.Compare = Scheme.mono $ a `Type.Function` a `Type.Function` Type.Constructor Type2.Ordering
        go Method.LessThen = Scheme.mono $ a `Type.Function` a `Type.Function` Type.Constructor Type2.Bool
        go Method.LessThenEqual = Scheme.mono $ a `Type.Function` a `Type.Function` Type.Constructor Type2.Bool
        go Method.GreaterThen = Scheme.mono $ a `Type.Function` a `Type.Function` Type.Constructor Type2.Bool
        go Method.GreaterThenEqual = Scheme.mono $ a `Type.Function` a `Type.Function` Type.Constructor Type2.Bool
        go Method.Max = Scheme.mono $ a `Type.Function` a `Type.Function` a
        go Method.Min = Scheme.mono $ a `Type.Function` a `Type.Function` a

ordExtra :: ClassExtra scope
ordExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go ord =
          Hook {hook = DefaultOrd {ord, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.empty
