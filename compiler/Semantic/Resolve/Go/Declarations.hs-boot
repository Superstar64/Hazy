{-# LANGUAGE RoleAnnotations #-}

module Semantic.Resolve.Go.Declarations where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import Semantic.Scope (Declaration, Environment (..))
import Semantic.Stage (Resolve)
import {-# SOURCE #-} Semantic.Tree.Declarations (Declarations)
import Syntax.Position (Position)
import qualified Syntax.Tree.Declarations as Syntax (Declarations)

resolve ::
  Context scope ->
  Syntax.Declarations Position ->
  ( Context (Declaration ':+ scope),
    Declarations locality Normal Resolve (Declaration ':+ scope)
  )
