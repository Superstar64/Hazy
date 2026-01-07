module Stage3.Simple.Constraint where

import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type as Type (unlocal)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Simple.Type (Type (Call, Variable))
import qualified Stage3.Simple.Type as Type (instanciate', lift)
import qualified Stage3.Tree.Constraint as Solved
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Prelude hiding (head)

data Constraint scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type (Local ':+ scope)))
  }
  deriving (Show)

argument :: Constraint scope -> Type (Local ':+ scope)
argument Constraint {head, arguments} =
  foldl Call (Variable (Local.Local head)) arguments

lift :: Constraint scope -> Unify.Constraint s scope
lift Constraint {classx, head, arguments} =
  Unify.constraintx
    classx
    head
    (Type.lift <$> arguments)

instanciate :: Strict.Vector (Unify.Type s scope) -> Constraint (Local ':+ scope) -> Unify.Constraint s scope
instanciate fresh Constraint {classx, head, arguments} =
  Unify.constraintx
    (Type2.map Type.unlocal classx)
    head
    (Type.instanciate' fresh <$> arguments)

instance Shift Constraint where
  shift = shiftDefault

instance Shift.Functor Constraint where
  map category Constraint {classx, head, arguments} =
    Constraint
      { classx = Shift.map category classx,
        head,
        arguments = Shift.map (Shift.Over category) <$> arguments
      }

simplify :: Solved.Constraint scope -> Constraint scope
simplify Solved.Constraint {Solved.classx, Solved.head, Solved.arguments'} = do
  Constraint
    { classx,
      head,
      arguments = arguments'
    }
