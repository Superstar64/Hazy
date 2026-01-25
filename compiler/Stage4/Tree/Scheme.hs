module Stage4.Tree.Scheme (Scheme (..), mono, constraintCount, simplify) where

import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Scheme as Solved
import qualified Stage3.Tree.TypePattern as Solved.TypePattern
import qualified Stage4.Shift as Shift2
import qualified Stage4.Tree.Constraint as Constraint
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type

newtype Scheme scope = Scheme
  { runScheme :: SchemeOver Type scope
  }
  deriving (Show)

instance Shift Scheme where
  shift = shiftDefault

instance Shift.Functor Scheme where
  map = Shift2.mapDefault

instance Shift2.Functor Scheme where
  map category (Scheme scheme) = Scheme (Shift2.map category scheme)

instance Scope.Show Scheme where
  showsPrec = showsPrec

mono :: Type scope -> Scheme scope
mono = Scheme . SchemeOver.mono

constraintCount :: Scheme scope -> Int
constraintCount (Scheme scheme) = SchemeOver.constraintCount scheme

simplify :: Solved.Scheme scope -> Scheme scope
simplify Solved.Scheme {parameters, constraints, result}
  | parameters <- fmap Solved.TypePattern._type parameters,
    constraints <- fmap Constraint.simplify constraints,
    result <- Type.simplify result =
      Scheme
        SchemeOver
          { parameters,
            constraints,
            result
          }
