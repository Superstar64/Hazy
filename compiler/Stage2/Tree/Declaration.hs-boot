{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Declaration where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Stage (Check, Stage)
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

type role Declaration nominal nominal nominal nominal

type Declaration :: Locality -> Layout -> Stage -> Environment -> Type
data Declaration locality layout stage scope

typex' :: Declaration locality layout Check scope -> Simple.Scheme scope
