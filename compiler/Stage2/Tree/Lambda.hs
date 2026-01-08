{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Lambda where

import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (..))
import qualified Stage1.Tree.Pattern as Stage1.Pattern
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)

data Lambda scope
  = Plain
      { plain :: !(Expression scope)
      }
  | Bound
      { boundPosition :: !Position,
        parameter :: !(Pattern scope),
        body :: !(Lambda (Scope.Pattern ':+ scope))
      }
  deriving (Show)

instance Shift Lambda where
  shift = shiftDefault

instance Shift.Functor Lambda where
  map category = \case
    Plain {plain} -> Plain {plain = Shift.map category plain}
    Bound {boundPosition, parameter, body} ->
      Bound
        { boundPosition,
          parameter = Shift.map category parameter,
          body = Shift.map (Shift.Over category) body
        }

-- todo complain when lambda variables shadow other lambda variables
resolve :: Context scope -> [Stage1.Pattern Position] -> Stage1.Expression Position -> Lambda scope
resolve context patterns expression = case patterns of
  [] -> Plain {plain = Expression.resolve context expression}
  (patternx : patterns)
    | parameter <- Pattern.resolve context patternx ->
        Bound
          { boundPosition = Stage1.Pattern.startPosition patternx,
            parameter,
            body = resolve (Pattern.augment parameter context) patterns expression
          }
