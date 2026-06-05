module Stage2.Unify.Constraint where

import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift
import Stage2.Unify.Class (Functor (..), Solve, Zonk (..))
import qualified Stage2.Unify.Class as Class
import {-# SOURCE #-} Stage2.Unify.Type (Type)
import {-# SOURCE #-} qualified Stage2.Unify.Type as Type
import qualified Stage4.Tree.Constraint as Simple
import Prelude hiding (Functor, map)

data Constraint s scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: Int,
    arguments :: !(Strict.Vector (Type s (Scope.Local ':+ scope)))
  }

instance Zonk Constraint where
  zonk zonker Constraint {classx, head, arguments} = do
    arguments <- traverse (zonk zonker) arguments
    pure Constraint {classx, head, arguments}

instance Shift (Constraint s) where
  shift = Class.shiftDefault

instance Functor (Constraint s) where
  map category Constraint {classx, head, arguments} =
    Constraint
      { classx = Shift.map (Class.general category) classx,
        head,
        arguments = Class.map (Class.Over category) <$> arguments
      }

solve :: Position -> Constraint s scope -> Solve s (Simple.Constraint scope)
solve position Constraint {classx, head, arguments} = do
  arguments <- traverse (Type.solve position) arguments
  pure $
    Simple.Constraint
      { classx,
        head,
        arguments
      }
