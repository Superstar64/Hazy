{-# LANGUAGE RoleAnnotations #-}

module Stage2.Resolve.Go.Type where

import Stage1.Position (Position)
import qualified Stage1.Tree.Type as Stage1 (Type)
import {-# SOURCE #-} Stage2.Resolve.Context (Context)
import Stage2.Stage (Resolve)
import Stage2.Tree.Type (Type)

resolve :: Context scope -> Stage1.Type Position -> Type Position Resolve scope
