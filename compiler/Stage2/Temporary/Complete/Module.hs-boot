module Stage2.Temporary.Complete.Module where

import Data.Vector (Vector)
import Stage1.Position (Position)
import qualified Stage1.Tree.Module as Stage1
import {-# SOURCE #-} qualified Stage2.Tree.Module as Real
import Verbose (Debug)

data Module

resolve :: (Debug verbose) => Vector (Stage1.Module Position) -> verbose (Vector Module)
shrink :: Module -> Real.Module
