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
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment ((:+)), Local)
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (), shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Check.Context (Context (..))
import qualified Stage3.Simple.Constraint as Constraint
import Stage3.Simple.SchemeOver (SchemeOver (..), augment)
import qualified Stage3.Simple.SchemeOver as SchemeOver
import Stage3.Simple.Type (Type)
import qualified Stage3.Simple.Type as Type
import qualified Stage3.Tree.Scheme as Solved
import {-# SOURCE #-} qualified Stage3.Tree.TypePattern as Solved.TypePattern
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Prelude hiding (head)

newtype Scheme scope = Scheme
  { runScheme :: SchemeOver Type scope
  }
  deriving (Show)

instance Shift Scheme where
  shift = shiftDefault

instance Shift.Functor Scheme where
  map category (Scheme scheme) = Scheme (Shift.map category scheme)

instance Term.Functor Scheme where
  map Term.Category {general} = Shift.map general

instance Scope.Show Scheme where
  showsPrec = showsPrec

mono :: Type scope -> Scheme scope
mono = Scheme . SchemeOver.mono

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

simplify :: Solved.Scheme scope -> Scheme scope
simplify Solved.Scheme {parameters, constraints, result}
  | parameters <- fmap Solved.TypePattern._type parameters,
    constraints <- fmap Constraint.simplify constraints,
    result <- Type.simplify result =
      Scheme
        SchemeOver
          { parameters,
            constraints,
            result
          }

augment' :: Position -> Scheme scope -> Context s scope -> ST s (Context s (Local ':+ scope))
augment' position = SchemeOver.augment' position . runScheme

constraintCount :: Scheme scope -> Int
constraintCount (Scheme scheme) = SchemeOver.constraintCount scheme
