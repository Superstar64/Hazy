{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Declarations where

import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Declaration (Declaration)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Void (Void)
import qualified Semantic.Check.Go.Declarations as Semantic
import Semantic.Layout (Normal)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)

type role Declarations nominal

type Declarations :: Environment -> Type
data Declarations scope

instance Show (Declarations scope)

instance Shift0.Functor Declarations

instance Shift.Functor Declarations

instance Substitute.Functor Declarations

simplify :: Semantic.Declarations Identity Void locality Identity Normal Check scope -> Declarations scope
single :: Declaration scope -> Declarations scope
