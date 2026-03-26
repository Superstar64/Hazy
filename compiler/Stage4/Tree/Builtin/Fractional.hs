module Stage4.Tree.Builtin.Fractional where

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

fractional :: Class scope
fractional =
  Class
    { parameter = Type.smallType,
      constraints =
        Strict.Vector.singleton
          Constraint
            { classx = Type2.Num,
              head = 0,
              arguments = Strict.Vector.empty
            },
      methods = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        a = Type.Variable (Local.Local 0)
        go Method.Divide = Scheme.mono $ a `Type.Function` a `Type.Function` a
        go Method.Recip = Scheme.mono $ a `Type.Function` a
        go Method.FromRational =
          Scheme.mono $
            Type.Constructor Type2.Ratio
              `Type.Call` Type.Constructor Type2.Integer
              `Type.Function` a

fractionalExtra :: ClassExtra scope
fractionalExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go fractional =
          Hook {hook = DefaultFractional {fractional, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.empty
