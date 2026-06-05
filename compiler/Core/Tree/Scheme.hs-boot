{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Scheme where

import qualified Core.Shift as Shift2
import Core.Tree.SchemeOver (SchemeOver)
import Core.Tree.Type (Type)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import {-# SOURCE #-} qualified Semantic.Tree.Scheme as Solved

type role Scheme nominal

newtype Scheme scope = Scheme
  { runScheme :: SchemeOver Type scope
  }

instance Scope.Show Scheme

instance Show (Scheme scope)

instance Shift Scheme

instance Shift.Functor Scheme

instance Shift2.Functor Scheme

simplify :: Solved.Scheme position Check scope -> Scheme scope
