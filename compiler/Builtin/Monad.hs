module Builtin.Monad where

import Builtin (builtinClass)
import Core.Tree.Class (Class (..))
import Core.Tree.ClassExtra (ClassExtra (..))
import Core.Tree.Evidence (Evidence (Variable, instanciation, variable))
import Core.Tree.Expression (Expression (Hook, hook))
import Core.Tree.Hook (Hook (..))
import qualified Core.Tree.Instanciation as Instanciation
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
(bindings, definition) =
  Type2.Monad
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Applicative m => Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        (>>) :: m a -> m b -> m b
        return :: a -> m a
      infixl 1 >>=, >>
      """

extra =
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
            instanciation = Instanciation.Mono
