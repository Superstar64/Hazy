module Core.Tree.Builtin.Fractional where

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
            instanciation = Instanciation.Mono
