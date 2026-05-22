{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Declarations where

import Data.Kind (Type)
import Stage2.Layout (Normal)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Declarations as Stage3
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import {-# SOURCE #-} Stage4.Tree.Declaration (Declaration)

type role Declarations nominal

type Declarations :: Environment -> Type
data Declarations scope

instance Show (Declarations scope)

instance Shift Declarations

instance Shift.Functor Declarations

instance Shift2.Functor Declarations

instance Substitute.Functor Declarations

simplify :: Stage3.Declarations locality Normal scope -> Declarations scope
single :: Declaration scope -> Declarations scope
