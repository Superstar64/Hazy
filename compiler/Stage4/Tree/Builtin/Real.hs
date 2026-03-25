module Stage4.Tree.Builtin.Real where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type2 as Type2
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import Stage4.Tree.Class (Class (..))
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Constraint (Constraint (..))
import Stage4.Tree.Evidence (Evidence (..))
import Stage4.Tree.Expression (Expression (Hook, hook))
import Stage4.Tree.Hook (Hook (..))
import qualified Stage4.Tree.Instanciation as Instanciation (empty)
import qualified Stage4.Tree.Scheme as Scheme
import qualified Stage4.Tree.Type as Type

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
            instanciation = Instanciation.empty
