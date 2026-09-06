{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.TypeDeclaration where

import {-# SOURCE #-} Core.Tree.Class (Class)
import {-# SOURCE #-} Core.Tree.Data (Data)
import Data.Functor.Identity (Identity)
import qualified Data.Kind
import {-# SOURCE #-} qualified Semantic.Check.Go.TypeDeclaration as Solved
import Semantic.Layout (Normal)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)

type role TypeDeclaration nominal

type TypeDeclaration :: Environment -> Data.Kind.Type
data TypeDeclaration scope

assumeData :: TypeDeclaration scope -> Data scope
assumeClass :: TypeDeclaration scope -> Class scope

instance Shift0.Functor TypeDeclaration

instance Shift.Functor TypeDeclaration

simplify :: Solved.TypeDeclaration locality Identity Normal Check scope -> TypeDeclaration scope
