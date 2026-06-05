{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Go.Type where

import {-# SOURCE #-} Semantic.Resolve.Context (Context)
import Semantic.Stage (Resolve)
import Semantic.Tree.Type (Type)
import Syntax.Position (Position)
import qualified Syntax.Tree.Type as Syntax (Type)

resolve :: Context scope -> Syntax.Type Position -> Type Position Resolve scope
