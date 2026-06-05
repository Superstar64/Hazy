{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Go.Pattern where

import Semantic.Resolve.Context (Context)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} Semantic.Tree.Pattern (Pattern)
import Syntax.Position (Position)
import qualified Syntax.Tree.Pattern as Syntax (Pattern)

resolve :: Context scope -> Syntax.Pattern Position -> Pattern Resolve scope
