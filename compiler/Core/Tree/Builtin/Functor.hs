module Core.Tree.Builtin.Functor where

import Core.Tree.Class (Class (..))
import Core.Tree.ClassExtra (ClassExtra (..))
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
import Semantic.Shift (shift)

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
functorExtra =
  ClassExtra
    { defaults = Strict.Vector.fromList set
    }
  where
    set = map go [minBound .. maxBound]
      where
        go functor =
          Hook {hook = DefaultFunctor {functor, evidence = Variable {variable, instanciation}}}
          where
            variable = Evidence.Index $ Evidence0.Shift $ Evidence0.Assumed 0
            instanciation = Instanciation.empty
