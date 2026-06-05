{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Definition3 where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)
import {-# SOURCE #-} Semantic.Tree.Definition2 (Mark)

type role Definition3 nominal nominal nominal nominal

type Definition3 :: Mark -> Layout -> Stage -> Environment -> Type
data Definition3 mark layout stage scope
