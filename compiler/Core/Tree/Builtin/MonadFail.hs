module Core.Tree.Builtin.MonadFail where

import Core.Tree.Class (Class (..))
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Constraint (Constraint (..))
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (..))
import qualified Core.Tree.Instanciation as Instanciation
import Core.Tree.Scheme (Scheme (..))
import Core.Tree.SchemeOver (SchemeOver (..))
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2
import Semantic.Shift (shift)

monadFail =
  Class
    { parameter = Type.smallType `Type.Function` Type.smallType,
      constraints =
        Strict.Vector.singleton
          Constraint
            { classx = Type2.Monad,
              head = 0,
              arguments = Strict.Vector.empty
            },
      methods = Strict.Vector.fromList $ map go [minBound .. maxBound]
    }
  where
    f = shift $ Type.Variable (Local.Local 0)
    a = Type.Variable (Local.Local 0)
    go Method.Fail =
      Scheme
        SchemeOver
          { parameters = Strict.Vector.singleton Type.smallType,
            constraints = Constraints.None,
            result =
              Type.Constructor Type2.List
                `Type.Call` Type.Constructor Type2.Char
                `Type.Function` f
                `Type.Call` a
          }

monadFailExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go monadFail =
          Hook {hook = DefaultMonadFail {monadFail, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
