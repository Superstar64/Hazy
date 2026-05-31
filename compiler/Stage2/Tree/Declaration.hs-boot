{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Declaration where

import Data.Kind (Type)
import Stage2.Layout (Layout, Normal)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Resolve, Stage)
import {-# SOURCE #-} Stage2.Tree.Definition2 (Mark (Inferred))
import {-# SOURCE #-} Stage2.Tree.Definition3 (Definition3)
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

type role Declaration nominal nominal nominal nominal

type Declaration :: Locality -> Layout -> Stage -> Environment -> Type
data Declaration locality layout stage scope

typex' :: Declaration locality layout Check scope -> Simple.Scheme scope

newtype Groupable scope
  = Groupable
  { element :: Definition3 'Inferred Normal Resolve scope
  }
