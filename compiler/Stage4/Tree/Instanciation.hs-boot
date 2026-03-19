{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Instanciation where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute

type role Instanciation nominal

type Instanciation :: Environment -> Type
data Instanciation scope

instance Show (Instanciation scope)

instance Shift Instanciation

instance Shift.Functor Instanciation

instance Shift2.Functor Instanciation

instance Substitute.Functor Instanciation

null :: Instanciation scope -> Bool
empty :: Instanciation scope
