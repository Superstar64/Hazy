{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.TypePattern where

import {-# SOURCE #-} qualified Core.Tree.Type as Simple (Type)
import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Stage)

type role TypePattern representational nominal nominal

type TypePattern :: Type -> Stage -> Environment -> Type
data TypePattern position stage scope

typex' :: TypePattern position Check scope -> Simple.Type scope
