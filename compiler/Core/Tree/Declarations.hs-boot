{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Declarations where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Declaration (Declaration)
import Data.Kind (Type)
import qualified Semantic.Check.Go.Declarations as Semantic
import Semantic.Layout (Normal)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)

type role Declarations nominal

type Declarations :: Environment -> Type
data Declarations scope

instance Show (Declarations scope)

instance Shift Declarations

instance Shift.Functor Declarations

instance Shift2.Functor Declarations

instance Substitute.Functor Declarations

simplify :: Semantic.Declarations locality Normal Check scope -> Declarations scope
single :: Declaration scope -> Declarations scope
