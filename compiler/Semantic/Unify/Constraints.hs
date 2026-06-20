module Semantic.Unify.Constraints where

import qualified Core.Tree.Constraints as Simple
import qualified Data.Vector.Strict as Strict
import Semantic.Shift (Shift (..))
import Semantic.Unify.Class (Functor (..), Solve, Zonk (..), shiftDefault)
import qualified Semantic.Unify.Class as Class
import Semantic.Unify.Constraint (Constraint)
import qualified Semantic.Unify.Constraint as Constraint
import Syntax.Position (Position)
import Prelude hiding (Functor (..))

data Constraints s scope
  = Constraints !(Strict.Vector (Constraint s scope))
  | None

instance Zonk Constraints where
  zonk zonker = \case
    Constraints constraints -> Constraints <$> traverse (zonk zonker) constraints
    None -> pure None

instance Shift (Constraints s) where
  shift = shiftDefault

instance Functor (Constraints s) where
  map category = \case
    Constraints constraints -> Constraints $ Class.map category <$> constraints
    None -> None

solve :: Position -> Constraints s scope -> Solve s (Simple.Constraints scope)
solve position = \case
  Constraints constraints -> Simple.Constraints <$> traverse (Constraint.solve position) constraints
  None -> pure Simple.None
