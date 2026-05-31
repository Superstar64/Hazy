{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.TypeDeclarationExtra where

import Data.Kind (Type)
import Stage2.Connect (Connect)
import Stage2.Layout (Layout)
import Stage2.Scope (Environment)
import Stage2.Stage (Stage)

type role TypeDeclarationExtra nominal nominal nominal

type TypeDeclarationExtra :: Layout -> Stage -> Environment -> Type
data TypeDeclarationExtra layout stage scope

instance Connect TypeDeclarationExtra
