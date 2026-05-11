{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Declaration where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import {-# SOURCE #-} qualified Stage4.Tree.Scheme as Simple

type role Declaration nominal

type Declaration :: Environment -> Type
data Declaration scope

typex_ :: Declaration scope -> Simple.Scheme scope
