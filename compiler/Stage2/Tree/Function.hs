{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Function where

import Stage1.Position (Position)
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (startPosition))
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.RightHandSide as RightHandSide (resolve)

data Function scope
  = Plain
      {rightHandSide :: !(RightHandSide scope)}
  | Bound
      { functionPosition :: !Position,
        patternx :: !(Pattern scope),
        function :: !(Function (Scope.Pattern ':+ scope))
      }
  deriving (Show)

instance Shift Function where
  shift = shiftDefault

instance Shift.Functor Function where
  map category = \case
    Plain {rightHandSide} -> Plain {rightHandSide = Shift.map category rightHandSide}
    Bound {functionPosition, patternx, function} ->
      Bound
        { functionPosition,
          patternx = Shift.map category patternx,
          function = Shift.map (Shift.Over category) function
        }

newtype Resolve = Resolve
  { patterns :: [Stage1.Pattern Position]
  }

-- todo complain when lambda variables shadow other lambda variables
resolve :: Context scope -> Resolve -> Stage1.RightHandSide Position -> Function scope
resolve context Resolve {patterns} rightHandSide1 = case patterns of
  [] -> Plain {rightHandSide = RightHandSide.resolve context rightHandSide1}
  (pattern1 : patterns) ->
    Bound
      { functionPosition = Stage1.startPosition pattern1,
        patternx,
        function = resolve (Pattern.augment patternx context) Resolve {patterns} rightHandSide1
      }
    where
      patternx = Pattern.resolve context pattern1
