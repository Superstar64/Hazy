{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Declaration where

import {-# SOURCE #-} qualified Core.Tree.Forall as Simple
import qualified Core.Tree.Type as Simple
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Semantic.Layout (Layout, Normal)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment)
import Semantic.Stage (Check, Resolve, Stage)
import {-# SOURCE #-} Semantic.Tree.Definition2 (Mark (Inferred))
import {-# SOURCE #-} Semantic.Tree.Definition3 (Definition3)

type role Declaration representational nominal representational nominal nominal nominal nominal

type Declaration :: (Type -> Type) -> Type -> (Type -> Type) -> Locality -> Layout -> Stage -> Environment -> Type
data Declaration solve logical loeb locality layout stage scope

typex' :: Declaration solve logical Identity locality layout Check scope -> Simple.ForallOver Simple.TypeF logical scope

newtype Groupable scope
  = Groupable
  { element :: Definition3 'Inferred Normal Resolve scope
  }
