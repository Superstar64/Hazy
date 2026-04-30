{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Shared where

import qualified Graph.StronglyConnected as StronglyConnected
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.Group.Index.Link.Term as Group.Term
import Stage2.Layout (Group, Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Inferred, Share)
import qualified Stage2.Tree.Definition2 as Definition2
import Stage2.Tree.Definition3 (Definition3)
import qualified Stage2.Tree.Definition3 as Definition3
import Stage2.Tree.Pattern (Pattern)

data Shared locality layout scope = Shared
  { equalPosition :: !Position,
    patternx :: !(Pattern scope),
    definition :: !(Definition3 locality Share Inferred layout scope)
  }
  deriving (Show)

instance Shift (Shared locality layout) where
  shift = shiftDefault

instance Shift.Functor (Shared locality layout) where
  map category Shared {equalPosition, patternx, definition} =
    Shared
      { equalPosition,
        patternx = Shift.map category patternx,
        definition = Shift.map category definition
      }

instance FreeTermVariables (Shared locality layout) where
  freeTermVariables target Shared {definition} = freeTermVariables target definition

group ::
  (Group.Term.Link locality -> Definition2.Auto scope) ->
  StronglyConnected.Component (Group.Term.Link locality) ->
  Shared locality Normal scope ->
  Shared locality Group scope
group index group Shared {equalPosition, patternx, definition} =
  Shared
    { equalPosition,
      patternx,
      definition = Definition3.group index group definition
    }
