module Core.Tree.Builtin.Ord where

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
            instanciation = Instanciation.Mono
