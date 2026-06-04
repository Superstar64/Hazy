{-# LANGUAGE RoleAnnotations #-}

module Stage2.Resolve.Go.Declarations where

import Stage1.Position (Position)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations)
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment (..))
import Stage2.Stage (Resolve)
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Declaration ':+ scope),
    Declarations locality Normal Resolve (Declaration ':+ scope)
  )
