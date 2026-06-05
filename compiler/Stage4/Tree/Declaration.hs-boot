{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Declaration where

import Data.Kind (Type)
import Stage2.Layout (Normal)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import Stage2.Stage (Check)
import qualified Stage2.Tree.Expression as Stage3
import qualified Stage2.Check.Go.Scheme as Stage3
import Stage4.Tree.SchemeOver (SchemeOver)

type role Declaration nominal

type Declaration :: Environment -> Type
data Declaration scope

instance Shift Declaration

annotation ::
  SchemeOver (Stage3.Expression Normal Check) scope ->
  Stage3.Scheme position Check scope ->
  Declaration scope
