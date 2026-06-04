{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Module where

import Data.Vector (Vector)
import Error (Position)
import qualified Stage1.Tree.Module as Stage1 (Module)
import Stage2.Layout (Normal)
import Stage2.Stage (Resolve)
import qualified Stage2.Temporary.Complete.Module as Complete
import Stage2.Tree.Module (Module (..))
import Verbose (Debug)

resolve :: (Debug verbose) => Vector (Stage1.Module Position) -> verbose (Vector (Module Normal Resolve))
resolve modules = fmap Complete.shrink <$> Complete.resolve modules
