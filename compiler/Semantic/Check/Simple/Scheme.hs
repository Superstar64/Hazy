module Semantic.Check.Simple.Scheme
  ( lift,
    instanciate,
    augment,
    augment',
    instanciate',
  )
where

import Control.Monad.ST (ST)
import Core.Tree.Scheme (Scheme (..))
import Core.Tree.SchemeOver (SchemeOver (..))
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context (..))
import Semantic.Check.Mask (Mask)
import qualified Semantic.Check.Simple.Constraint as Constraint
import Semantic.Check.Simple.SchemeOver (augment)
import qualified Semantic.Check.Simple.SchemeOver as SchemeOver
import qualified Semantic.Check.Simple.Type as Type
import Semantic.Scope (Environment ((:+)), Local)
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (head)

lift :: Scheme scope -> Unify.Scheme s scope
lift = Unify.Scheme . SchemeOver.lift (SchemeOver.Lift Type.lift) . runScheme

instanciate :: Context s scope -> Position -> Scheme scope -> ST s (Unify.Type s scope, Unify.Instanciation s scope)
instanciate context position = Unify.instanciate context position . lift

instanciate' :: Strict.Vector (Unify.Type s scope) -> Scheme (Local ':+ scope) -> Unify.Scheme s scope
instanciate' fresh (Scheme SchemeOver {parameters, constraints, result}) =
  Unify.scheme
    (Type.instanciate fresh <$> parameters)
    (Constraint.instanciate fresh <$> constraints)
    (Type.instanciate' fresh result)

augment' :: Position -> Scheme scope -> Mask -> Context s scope -> ST s (Context s (Local ':+ scope))
augment' position = SchemeOver.augment' position . runScheme
