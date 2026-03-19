module Stage4.Tree.Builtin.Monad where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (shift)
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import Stage4.Tree.Class (Class (..))
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Constraint (Constraint (..))
import Stage4.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Stage4.Tree.Expression (Expression (Hook, hook))
import Stage4.Tree.Hook (Hook (..))
import qualified Stage4.Tree.Instanciation as Instanciation
import Stage4.Tree.Scheme (Scheme (..))
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.Type as Type

monad =
  Class
    { parameter = Type.smallType `Type.Function` Type.smallType,
      constraints =
        Strict.Vector.singleton
          Constraint
            { classx = Type2.Applicative,
              head = 0,
              arguments = Strict.Vector.empty
            },
      methods = Strict.Vector.fromList $ map go [minBound .. maxBound]
    }
  where
    f = shift $ Type.Variable (Local.Local 0)
    a = Type.Variable (Local.Local 0)
    b = Type.Variable (Local.Local 1)
    go = \case
      Method.Bind ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Strict.Vector.empty,
              result =
                f
                  `Type.Call` a
                  `Type.Function` (a `Type.Function` f `Type.Call` b)
                  `Type.Function` f
                  `Type.Call` b
            }
      Method.Then ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Strict.Vector.empty,
              result =
                f `Type.Call` a `Type.Function` f `Type.Call` b `Type.Function` f `Type.Call` b
            }
      Method.Return ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.singleton Type.smallType,
              constraints = Strict.Vector.empty,
              result = a `Type.Function` f `Type.Call` a
            }

monadExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go monad =
          Hook {hook = DefaultMonad {monad, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.empty
