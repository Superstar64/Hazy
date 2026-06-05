{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Lambda where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context (..))
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Expression as Expression (resolve)
import qualified Semantic.Resolve.Go.Pattern as Pattern (augment, resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Lambda (Lambda (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Expression as Syntax (Expression (..))
import qualified Syntax.Tree.Pattern as Syntax (Pattern (..))
import qualified Syntax.Tree.Pattern as Syntax.Pattern

-- todo complain when lambda variables shadow other lambda variables
resolve :: Context scope -> [Syntax.Pattern Position] -> Syntax.Expression Position -> Lambda Normal Resolve scope
resolve context patterns expression = case patterns of
  [] -> Plain {plain = Expression.resolve context expression}
  (patternx : patterns)
    | parameter <- Pattern.resolve context patternx ->
        Bound
          { boundPosition = Syntax.Pattern.startPosition patternx,
            parameter,
            body = resolve (Pattern.augment parameter context) patterns expression
          }
