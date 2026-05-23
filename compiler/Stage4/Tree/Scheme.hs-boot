{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Scheme where

import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import {-# SOURCE #-} qualified Stage3.Tree.Scheme as Solved
import qualified Stage4.Shift as Shift2
import Stage4.Tree.SchemeOver (SchemeOver)
import Stage4.Tree.Type (Type)

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
