{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Declaration where

import {-# SOURCE #-} qualified Core.Tree.Scheme as Simple
import Data.Kind (Type)
import Semantic.Layout (Layout, Normal)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve, Stage)
import {-# SOURCE #-} Semantic.Tree.Definition2 (Mark (Inferred))
import {-# SOURCE #-} Semantic.Tree.Definition3 (Definition3)

type role Declaration nominal nominal nominal nominal

type Declaration :: Locality -> Layout -> Stage -> Environment -> Type
data Declaration locality layout stage scope

typex' :: Declaration locality layout Check scope -> Simple.Scheme scope

newtype Groupable scope
  = Groupable
  { element :: Definition3 'Inferred Normal Resolve scope
  }
