{-# LANGUAGE RoleAnnotations #-}

module Stage2.Resolve.Go.Pattern where

import Stage1.Position (Position)
import qualified Stage1.Tree.Pattern as Stage1 (Pattern)
import Stage2.Resolve.Context (Context)
import Stage2.Stage (Resolve)
import {-# SOURCE #-} Stage2.Tree.Pattern (Pattern)

resolve :: Context scope -> Stage1.Pattern Position -> Pattern Resolve scope
