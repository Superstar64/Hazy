module Semantic.Unify.Constraint where

import qualified Core.Tree.Constraint as Simple
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..))
import qualified Semantic.Shift as Shift
import Semantic.Unify.Class (Functor (..), Solve, Zonk (..))
import qualified Semantic.Unify.Class as Class
import {-# SOURCE #-} Semantic.Unify.Type (Type)
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)
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
