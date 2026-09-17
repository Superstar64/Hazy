module Semantic.Resolve.Temporary.Complete.Module where

import Data.Functor.Identity (Identity)
import Data.Vector (Vector)
import Semantic.Layout (Normal)
import Semantic.Scope (Global)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} qualified Semantic.Tree.Module as Real
import Syntax.Position (Position)
import qualified Syntax.Tree.Module as Syntax
import Verbose (Debug)

data Module

resolve :: (Debug verbose) => Vector (Syntax.Module Position) -> verbose (Vector Module)
shrink :: Module -> Real.Module Identity Normal Resolve Global
