{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Evidence as Solved (Evidence)
import qualified Data.Kind as Kind
import Semantic.Scope (Environment (..))
import Semantic.Unify.Class (Solve, Zonk)
import Syntax.Position (Position)

type role Evidence nominal nominal

type Evidence :: Kind.Type -> Environment -> Kind.Type
data Evidence s scope

instance Zonk Evidence

unify :: Evidence s scope -> Evidence s scope -> ST s ()
unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
solve :: Position -> Evidence s scope -> Solve s (Solved.Evidence scope)
