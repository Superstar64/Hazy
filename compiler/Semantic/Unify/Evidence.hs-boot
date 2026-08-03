{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (EvidenceF)
import qualified Data.Kind as Kind
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0

type Evidence s scope = EvidenceF (Logical s scope) scope

type role Logical nominal nominal

type Logical :: Kind.Type -> Environment -> Kind.Type
data Logical s scope

instance Shift0.Functor (Logical s)

unify :: Evidence s scope -> Evidence s scope -> ST s ()
unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
