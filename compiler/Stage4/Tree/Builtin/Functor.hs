module Stage4.Tree.Builtin.Functor where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Method as Method
import Stage2.Shift (shift)
import Stage4.Tree.Class (Class (..))
import Stage4.Tree.ClassExtra (ClassExtra (..))
import Stage4.Tree.Scheme (Scheme (..))
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.Type as Type
import Stage4.Tree.Evidence (Evidence (Variable, variable))
import Stage4.Tree.Expression (Expression(Hook, hook))
import Stage4.Tree.Hook (Hook(..))
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0

functor :: Class scope
functor =
  Class
    { parameter = Type.smallType `Type.Function` Type.smallType,
      constraints = Strict.Vector.empty,
      methods = Strict.Vector.fromList $ map go [minBound .. maxBound]
    }
  where
    f = shift $ Type.Variable (Local.Local 0)
    a = Type.Variable (Local.Local 0)
    b = Type.Variable (Local.Local 1)
    go = \case
      Method.Fmap ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Strict.Vector.empty,
              result =
                (a `Type.Function` b)
                  `Type.Function` f
                  `Type.Call` a
                  `Type.Function` f
                  `Type.Call` b
            }
      Method.Fconst ->
        Scheme
          SchemeOver
            { parameters = Strict.Vector.fromList [Type.smallType, Type.smallType],
              constraints = Strict.Vector.empty,
              result = a `Type.Function` f `Type.Call` b `Type.Function` f `Type.Call` b
            }

functorExtra :: ClassExtra scope
functorExtra =  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go functor =
          Hook {hook = DefaultFunctor {functor, evidence = Variable {variable}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
