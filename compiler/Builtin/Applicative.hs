module Builtin.Applicative where

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
  Type2.Applicative
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        liftA2 :: (a -> b -> c) -> f a -> f b -> f c
        (*>) :: f a -> f b -> f b
        (<*) :: fa -> f b -> f a
      infixl 4 <*>, *>, <*
      """

extra :: ClassExtra scope
extra =
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
