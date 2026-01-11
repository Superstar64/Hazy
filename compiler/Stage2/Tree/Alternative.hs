module Stage2.Tree.Alternative where

import Stage1.Position (Position)
import qualified Stage1.Tree.Alternative as Stage1 (Alternative (..))
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.RightHandSide as RightHandSide (resolve)

data Alternative scope
  = Alternative !(Pattern scope) !(RightHandSide (Scope.Pattern ':+ scope))
  deriving (Show)

instance Shift Alternative where
  shift = shiftDefault

instance Shift.Functor Alternative where
  map category (Alternative patternx rightHandSide) =
    Alternative
      (Shift.map category patternx)
      (Shift.map (Shift.Over category) rightHandSide)

resolve :: Context scope -> Stage1.Alternative Position -> Alternative scope
resolve context (Stage1.Alternative {parameter, rightHandSide}) =
  Alternative pattern' (RightHandSide.resolve (Pattern.augment pattern' context) rightHandSide)
  where
    pattern' = Pattern.resolve context parameter
