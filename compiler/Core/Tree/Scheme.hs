module Core.Tree.Scheme (Scheme (..), mono, constraintCount, simplify) where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (ConstraintCount)
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.SchemeOver (SchemeOver (..))
import qualified Core.Tree.SchemeOver as SchemeOver
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Semantic.Check.Go.Scheme as Solved
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.TypePattern as Solved.TypePattern

newtype Scheme scope = Scheme
  { runScheme :: SchemeOver Type scope
  }
  deriving (Show)

instance Shift Scheme where
  shift = shiftDefault

instance Shift.Functor Scheme where
  map = Shift2.mapDefault

instance Shift2.Functor Scheme where
  map = Substitute.mapDefault

instance Substitute.Functor Scheme where
  map category (Scheme scheme) = Scheme (Substitute.map category scheme)

instance Scope.Show Scheme where
  showsPrec = showsPrec

mono :: Type scope -> Scheme scope
mono = Scheme . SchemeOver.mono

constraintCount :: Scheme scope -> ConstraintCount
constraintCount (Scheme scheme) = SchemeOver.constraintCount scheme

simplify :: Solved.Scheme position Check scope -> Scheme scope
simplify Solved.Scheme {parameters, constraints, result}
  | parameters <- fmap Solved.TypePattern.typex' parameters,
    constraints <- Constraints.simplify constraints,
    result <- Type.simplify result =
      Scheme
        SchemeOver
          { parameters,
            constraints,
            result
          }
