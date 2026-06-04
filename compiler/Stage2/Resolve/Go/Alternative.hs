module Stage2.Resolve.Go.Alternative where

import Stage1.Position (Position)
import qualified Stage1.Tree.Alternative as Stage1 (Alternative (..))
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Resolve.Go.Pattern as Pattern (augment, resolve)
import qualified Stage2.Resolve.Go.RightHandSide as RightHandSide (resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Alternative (Alternative (..))

resolve :: Context scope -> Stage1.Alternative Position -> Alternative Normal Resolve scope
resolve context (Stage1.Alternative {parameter, rightHandSide}) =
  Alternative pattern' (RightHandSide.resolve (Pattern.augment pattern' context) rightHandSide)
  where
    pattern' = Pattern.resolve context parameter
