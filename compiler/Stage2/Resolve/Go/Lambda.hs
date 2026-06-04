{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Lambda where

import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (..))
import qualified Stage1.Tree.Pattern as Stage1.Pattern
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context (..))
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Expression as Expression (resolve)
import qualified Stage2.Resolve.Go.Pattern as Pattern (augment, resolve)
import Stage2.Stage (Resolve)
import Stage2.Tree.Lambda (Lambda (..))

-- todo complain when lambda variables shadow other lambda variables
resolve :: Context scope -> [Stage1.Pattern Position] -> Stage1.Expression Position -> Lambda Normal Resolve scope
resolve context patterns expression = case patterns of
  [] -> Plain {plain = Expression.resolve context expression}
  (patternx : patterns)
    | parameter <- Pattern.resolve context patternx ->
        Bound
          { boundPosition = Stage1.Pattern.startPosition patternx,
            parameter,
            body = resolve (Pattern.augment parameter context) patterns expression
          }
