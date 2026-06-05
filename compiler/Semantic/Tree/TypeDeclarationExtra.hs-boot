{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.TypeDeclarationExtra where

import Data.Kind (Type)
import Semantic.Connect (Connect)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role TypeDeclarationExtra nominal nominal nominal

type TypeDeclarationExtra :: Layout -> Stage -> Environment -> Type
data TypeDeclarationExtra layout stage scope

instance Connect TypeDeclarationExtra
