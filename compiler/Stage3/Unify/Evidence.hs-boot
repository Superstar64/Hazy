{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify.Evidence where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind
import Stage1.Position (Position)
import Stage2.Scope (Environment (..))
import Stage3.Unify.Class (Zonk)
import {-# SOURCE #-} qualified Stage4.Tree.Evidence as Solved (Evidence)

type role Evidence nominal nominal

type Evidence :: Kind.Type -> Environment -> Kind.Type
data Evidence s scope

instance Zonk Evidence

unify :: Evidence s scope -> Evidence s scope -> ST s ()
unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
solve :: Position -> Evidence s scope -> ST s (Solved.Evidence scope)
