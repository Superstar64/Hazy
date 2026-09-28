module Builtin.MonadFail where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Class scope
extra :: ClassExtra scope
(bindings, definition, extra) =
  Type2.MonadFail
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Monad m => MonadFail m where
        fail :: [Char] -> m a
      """
