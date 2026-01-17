{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Scheme where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Tree.Scheme as Solved
import qualified Stage4.Index.Term as Term

type role Scheme nominal

type Scheme :: Environment -> Type
data Scheme scope

instance Show (Scheme scope)

instance Shift Scheme

instance Shift.Functor Scheme

instance Term.Functor Scheme

simplify :: Solved.Scheme scope -> Scheme scope
