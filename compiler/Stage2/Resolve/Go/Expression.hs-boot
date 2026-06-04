{-# LANGUAGE RoleAnnotations #-}

module Stage2.Resolve.Go.Expression where

import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Stage (Resolve)
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)

resolve :: Context scope -> Stage1.Expression Position -> Expression Normal Resolve scope
