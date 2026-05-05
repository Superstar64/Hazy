module Stage2.Temporary.Complete.Shared where

import Stage1.Position (Position)
import Stage1.Tree.Declaration as Stage1 (Declaration (..))
import qualified Stage1.Tree.LeftHandSide as Stage1 (LeftHandSide (Pattern))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (..))
import qualified Stage1.Tree.RightHandSide as Stage1.RightHandSide
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Tree.Definition2 as Real.Definition2
import qualified Stage2.Tree.Definition3 as Real.Definition3
import qualified Stage2.Tree.Pattern as Pattern (resolve)
import qualified Stage2.Tree.RightHandSide as RightHandSide (resolve)
import qualified Stage2.Tree.Shared as Real

data Shared scope = Shared
  { share :: forall locality. Real.Shared locality Normal scope
  }

shrink :: Shared scope -> Real.Shared locality Normal scope
shrink = share

resolve :: Context scope -> [Declaration Position] -> [Shared scope]
resolve context (Stage1.Definition {leftHandSide = Stage1.Pattern pattern1, rightHandSide} : declarations)
  | Stage1.Variable {} <- pattern1 = resolve context declarations
  | otherwise = shared : resolve context declarations
  where
    shared =
      Shared
        { share =
            Real.Shared
              { equalPosition = Stage1.RightHandSide.equalPosition rightHandSide,
                patternx = Pattern.resolve context pattern1,
                definition =
                  Real.Definition3.Auto $
                    Real.Definition2.Shared $
                      RightHandSide.resolve context rightHandSide
              }
        }
resolve context (_ : declarations) = resolve context declarations
resolve _ [] = []
