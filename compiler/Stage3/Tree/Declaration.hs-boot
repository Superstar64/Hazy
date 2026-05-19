{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Declaration where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

type role Declaration phantom phantom nominal

type Declaration :: Locality -> Layout -> Environment -> Type
data Declaration locality layout scope

typex_ :: Declaration locality layout scope -> Simple.Scheme scope
