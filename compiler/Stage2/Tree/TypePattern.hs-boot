{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.TypePattern where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Stage)
import {-# SOURCE #-} qualified Stage4.Tree.Type as Simple (Type)

type role TypePattern representational nominal nominal

type TypePattern :: Type -> Stage -> Environment -> Type
data TypePattern position stage scope

typex' :: TypePattern position Check scope -> Simple.Type scope
