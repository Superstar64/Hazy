module Core.Tree.Builtin.Real where

import Core.Tree.Class (Class (..))
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Constraint (Constraint (..))
import Core.Tree.Evidence (Evidence (..))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (..))
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Scheme as Scheme
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2

real :: Class scope
real =
  Class
    { parameter = Type.smallType,
      constraints =
        Strict.Vector.fromList
          [ Constraint
              { classx = Type2.Num,
                head = 0,
                arguments = Strict.Vector.empty
              },
            Constraint
              { classx = Type2.Ord,
                head = 0,
                arguments = Strict.Vector.empty
              }
          ],
      methods = Strict.Vector.fromList $ map go [minBound .. maxBound]
    }
  where
    a = Type.Variable (Local.Local 0)
    go Method.ToRational =
      Scheme.mono $
        a
          `Type.Function` Type.Constructor Type2.Ratio
          `Type.Call` Type.Constructor Type2.Integer

realExtra :: ClassExtra scope
realExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go real =
          Hook {hook = DefaultReal {real, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
