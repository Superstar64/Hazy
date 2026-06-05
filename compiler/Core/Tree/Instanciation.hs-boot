{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Instanciation where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift

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
