module Stage2.Temporary.Complete.Shared where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Declaration as Stage1 (Declaration (..))
import qualified Stage1.Tree.LeftHandSide as Stage1 (LeftHandSide (Pattern))
import qualified Stage1.Tree.Marked as Marked
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (..))
import qualified Stage2.Tree.Pattern as Pattern (resolve)
import qualified Stage2.Tree.RightHandSide as RightHandSide (resolve)
import qualified Stage2.Tree.Shared as Real

data Shared scope = Shared
  { bindings :: !(Strict.Vector (Marked.Variable Position)),
    share :: Real.Shared scope
  }

shrink :: Shared scope -> Real.Shared scope
shrink = share

resolve context (Stage1.Definition {Stage1.leftHandSide = Stage1.Pattern pattern1, Stage1.rightHandSide} : declarations)
  | Stage1.Variable {} <- pattern1 = resolve context declarations
  | otherwise = shared : resolve context declarations
  where
    shared =
      Shared
        { bindings,
          share =
            Real.Shared
              { Real.patternx,
                Real.definition
              }
        }
    bindings = Strict.Vector.fromList $ termBindingVariables pattern1
    patternx = Pattern.resolve context pattern1
    definition = RightHandSide.resolve context rightHandSide
resolve context (_ : declarations) = resolve context declarations
resolve _ [] = []
