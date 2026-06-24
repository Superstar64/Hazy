module Builtin.Ratio where

import Builtin (builtinData)
import Core.Tree.Data (Data)
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

bindings :: Bindings () scope
definition :: Data scope
(bindings, definition) =
  Type2.Ratio
    `builtinData` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      data Ratio a = !a :% !a
      infixl 7 :%
      """
