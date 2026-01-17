{-# LANGUAGE RoleAnnotations #-}

module Stage4.Temporary.Declarations where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Declarations as Stage3
import qualified Stage4.Index.Term as Term
import qualified Stage4.Tree.Declarations as Real

type role Declarations nominal

type Declarations :: Environment -> Type
data Declarations scope

instance Show (Declarations scope)

instance Shift Declarations

instance Shift.Functor Declarations

instance Term.Functor Declarations

simplify :: Stage3.Declarations scope -> Declarations scope
finish :: Declarations scope -> Real.Declarations scope
