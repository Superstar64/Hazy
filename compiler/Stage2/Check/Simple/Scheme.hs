module Stage2.Check.Simple.Scheme
  ( lift,
    instanciate,
    augment,
    augment',
    instanciate',
  )
where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage2.Scope (Environment ((:+)), Local)
import {-# SOURCE #-} Stage2.Check.Context (Context (..))
import Stage2.Check.Mask (Mask)
import qualified Stage2.Check.Simple.Constraint as Constraint
import Stage2.Check.Simple.SchemeOver (augment)
import qualified Stage2.Check.Simple.SchemeOver as SchemeOver
import qualified Stage2.Check.Simple.Type as Type
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import Stage4.Tree.Scheme (Scheme (..))
import Stage4.Tree.SchemeOver (SchemeOver (..))
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
