{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Module where

import Data.Functor.Identity (Identity)
import Data.Vector (Vector)
import Error (Position)
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Temporary.Complete.Module as Complete
import Semantic.Scope (Global)
import Semantic.Stage (Resolve)
import Semantic.Tree.Module (Module (..))
import qualified Syntax.Tree.Module as Syntax (Module)
import Verbose (Debug)

resolve ::
  (Debug verbose) =>
  Vector (Syntax.Module Position) ->
  verbose (Vector (Module Identity Normal Resolve Global))
resolve modules = fmap Complete.shrink <$> Complete.resolve modules
