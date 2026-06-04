{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Function where

import Stage1.Position (Position)
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (startPosition))
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Resolve.Go.Pattern as Pattern (augment, resolve)
import qualified Stage2.Resolve.Go.RightHandSide as RightHandSide (resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Function (Function (..))

-- todo complain when lambda variables shadow other lambda variables
resolve ::
  Context scope ->
  [Stage1.Pattern Position] ->
  Stage1.RightHandSide Position ->
  Function Normal Resolve scope
resolve context patterns rightHandSide1 = case patterns of
  [] -> Plain {rightHandSide = RightHandSide.resolve context rightHandSide1}
  (pattern1 : patterns) ->
    Bound
      { functionPosition = Stage1.startPosition pattern1,
        patternx,
        function = resolve (Pattern.augment patternx context) patterns rightHandSide1
      }
    where
      patternx = Pattern.resolve context pattern1
