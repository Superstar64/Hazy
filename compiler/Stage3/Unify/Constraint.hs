module Stage3.Unify.Constraint where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage3.Unify.Class (Zonk (..))
import {-# SOURCE #-} Stage3.Unify.Type (Type)
import {-# SOURCE #-} qualified Stage3.Unify.Type as Type
import qualified Stage4.Tree.Constraint as Simple

data Constraint s scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: Int,
    arguments :: !(Strict.Vector (Type s (Scope.Local ':+ scope)))
  }

instance Zonk Constraint where
  zonk zonker Constraint {classx, head, arguments} = do
    arguments <- traverse (zonk zonker) arguments
    pure Constraint {classx, head, arguments}

solve :: Position -> Constraint s scope -> ST s (Simple.Constraint scope)
solve position Constraint {classx, head, arguments} = do
  arguments <- traverse (Type.solve position) arguments
  pure $
    Simple.Constraint
      { classx,
        head,
        arguments
      }
