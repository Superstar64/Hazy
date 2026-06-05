{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Go.Expression where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)
import Syntax.Position (Position)
import qualified Syntax.Tree.Expression as Syntax (Expression)

resolve :: Context scope -> Syntax.Expression Position -> Expression Normal Resolve scope
