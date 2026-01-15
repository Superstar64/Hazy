module Stage3.Simple.Scheme
  ( Scheme (..),
    mono,
    lift,
    instanciate,
    simplify,
    augment,
    augment',
    instanciate',
    constraintCount,
  )
where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage2.Scope (Environment ((:+)), Local)
import {-# SOURCE #-} Stage3.Check.Context (Context (..))
import qualified Stage3.Simple.Constraint as Constraint
import Stage3.Simple.SchemeOver (SchemeOver (..), augment)
import qualified Stage3.Simple.SchemeOver as SchemeOver
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify
-- todo, this should probably be an SOURCE import
-- but that doesn't seem to required at the moment
import Stage4.Tree.Scheme (Scheme (..), constraintCount, mono, simplify)
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

augment' :: Position -> Scheme scope -> Context s scope -> ST s (Context s (Local ':+ scope))
augment' position = SchemeOver.augment' position . runScheme
