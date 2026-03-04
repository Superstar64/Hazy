module Stage4.Tree.Builtin.Applicative where

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
import Stage4.Tree.Evidence (Evidence (Variable, variable))
import Stage4.Tree.Expression (Expression (Hook, hook))
import Stage4.Tree.Hook (Hook (..))
import Stage4.Tree.Scheme (Scheme (..))
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.Type as Type

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
              constraints = Strict.Vector.empty,
              result = a `Type.Function` f `Type.Call` a
            }
      Method.Ap ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Strict.Vector.empty,
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
              constraints = Strict.Vector.empty,
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
              constraints = Strict.Vector.empty,
              result =
                f `Type.Call` a `Type.Function` f `Type.Call` b `Type.Function` f `Type.Call` b
            }
      Method.DiscardRight ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Strict.Vector.empty,
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
          Hook {hook = DefaultApplicative {applicative, evidence = Variable {variable}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
