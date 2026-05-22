{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.TypeDeclaration where

import qualified Data.Kind
import Stage2.Layout (Normal)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclaration as Solved
import {-# SOURCE #-} Stage4.Tree.Class (Class)
import {-# SOURCE #-} Stage4.Tree.Data (Data)

type role TypeDeclaration nominal

type TypeDeclaration :: Environment -> Data.Kind.Type
data TypeDeclaration scope

assumeData :: TypeDeclaration scope -> Data scope
assumeClass :: TypeDeclaration scope -> Class scope

instance Shift TypeDeclaration

instance Shift.Functor TypeDeclaration

simplify :: Solved.TypeDeclaration locality Normal scope -> TypeDeclaration scope
