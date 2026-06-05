module Semantic.Resolve.Go.Alternative where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context (..))
import qualified Semantic.Resolve.Go.Pattern as Pattern (augment, resolve)
import qualified Semantic.Resolve.Go.RightHandSide as RightHandSide (resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Alternative (Alternative (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Alternative as Syntax (Alternative (..))

resolve :: Context scope -> Syntax.Alternative Position -> Alternative Normal Resolve scope
resolve context (Syntax.Alternative {parameter, rightHandSide}) =
  Alternative pattern' (RightHandSide.resolve (Pattern.augment pattern' context) rightHandSide)
  where
    pattern' = Pattern.resolve context parameter
