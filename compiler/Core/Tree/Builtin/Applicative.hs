module Core.Tree.Builtin.Applicative where

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

applicative :: Class scope
applicative =
  Class
    { parameter = Type.smallType `Type.Function` Type.smallType,
      constraints =
        Strict.Vector.singleton
          Constraint
            { classx = Type2.Functor,
              head = 0,
              arguments = Strict.Vector.empty
            },
      methods = Strict.Vector.fromList $ map go [minBound .. maxBound]
    }
  where
    f = shift $ Type.Variable (Local.Local 0)
    a = Type.Variable (Local.Local 0)
    b = Type.Variable (Local.Local 1)
    c = Type.Variable (Local.Local 2)
    go = \case
      Method.Pure ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType],
              constraints = Constraints.None,
              result = a `Type.Function` f `Type.Call` a
            }
      Method.Ap ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Constraints.None,
              result =
                f
                  `Type.Call` (a `Type.Function` b)
                  `Type.Function` f
                  `Type.Call` a
                  `Type.Function` f
                  `Type.Call` b
            }
      Method.LiftA2 ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType, Type.smallType],
              constraints = Constraints.None,
              result =
                (a `Type.Function` b `Type.Function` c)
                  `Type.Function` f
                  `Type.Call` a
                  `Type.Function` f
                  `Type.Call` b
                  `Type.Function` f
                  `Type.Call` c
            }
      Method.DiscardLeft ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Constraints.None,
              result =
                f `Type.Call` a `Type.Function` f `Type.Call` b `Type.Function` f `Type.Call` b
            }
      Method.DiscardRight ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Constraints.None,
              result =
                f `Type.Call` a `Type.Function` f `Type.Call` b `Type.Function` f `Type.Call` a
            }

applicativeExtra :: ClassExtra scope
applicativeExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go applicative =
          Hook {hook = DefaultApplicative {applicative, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.Mono
