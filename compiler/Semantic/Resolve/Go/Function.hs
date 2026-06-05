{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Function where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Pattern as Pattern (augment, resolve)
import qualified Semantic.Resolve.Go.RightHandSide as RightHandSide (resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Function (Function (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Pattern as Syntax (Pattern (startPosition))
import qualified Syntax.Tree.RightHandSide as Syntax (RightHandSide (..))

-- todo complain when lambda variables shadow other lambda variables
resolve ::
  Context scope ->
  [Syntax.Pattern Position] ->
  Syntax.RightHandSide Position ->
  Function Normal Resolve scope
resolve context patterns rightHandSide1 = case patterns of
  [] -> Plain {rightHandSide = RightHandSide.resolve context rightHandSide1}
  (pattern1 : patterns) ->
    Bound
      { functionPosition = Syntax.startPosition pattern1,
        patternx,
        function = resolve (Pattern.augment patternx context) patterns rightHandSide1
      }
    where
      patternx = Pattern.resolve context pattern1
