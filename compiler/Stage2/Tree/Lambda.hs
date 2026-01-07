{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Lambda where

import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (..))
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
  = Plain !(Expression scope)
  | Bound !(Pattern scope) (Lambda (Scope.Pattern ':+ scope))
  deriving (Show)

instance Shift Lambda where
  shift = shiftDefault

instance Shift.Functor Lambda where
  map category = \case
    Plain expression -> Plain (Shift.map category expression)
    Bound patternx lambda ->
      Bound
        (Shift.map category patternx)
        (Shift.map (Shift.Over category) lambda)

newtype Resolve = Resolve
  { patterns :: [Stage1.Pattern Position]
  }

-- todo complain when lambda variables shadow other lambda variables
resolve :: Context scope -> Resolve -> Stage1.Expression Position -> Lambda scope
resolve context Resolve {patterns} expression1 = case patterns of
  [] -> Plain (Expression.resolve context expression1)
  (pattern1 : patterns) -> Bound pattern' (resolve (Pattern.augment pattern' context) Resolve {patterns} expression1)
    where
      pattern' = Pattern.resolve context pattern1
