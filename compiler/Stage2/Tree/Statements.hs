module Stage2.Tree.Statements where

import Data.Foldable (toList)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression)
import qualified Stage1.Tree.Statement as Stage1 (Statement (..))
import qualified Stage1.Tree.Statements as Stage1 (Statements (..))
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Declaration, Pattern)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)

data Statements scope
  = Done !(Expression scope)
  | Run !(Expression scope) !(Statements scope)
  | Bind !(Pattern scope) !(Expression scope) !(Statements (Scope.Pattern ':+ scope))
  | Let !(Declarations (Scope.Declaration ':+ scope)) !(Statements (Scope.Declaration ':+ scope))
  deriving (Show)

instance Shift Statements where
  shift = shiftDefault

instance Shift.Functor Statements where
  map category = \case
    Done expression -> Done (Shift.map category expression)
    Run expression statements ->
      Run (Shift.map category expression) (Shift.map category statements)
    Bind patternx expression statements ->
      Bind
        (Shift.map category patternx)
        (Shift.map category expression)
        (Shift.map (Shift.Over category) statements)
    Let declarations statements ->
      Let (Shift.map (Shift.Over category) declarations) (Shift.map (Shift.Over category) statements)

resolve :: Context scope -> Stage1.Statements Position -> Statements scope
resolve context Stage1.Statements {body, done} = statements context (toList body) done
  where
    statements :: Context scope -> [Stage1.Statement Position] -> Stage1.Expression Position -> Statements scope
    statements context [] done = Done (Expression.resolve context done)
    statements context (Stage1.Run run : remaining) done =
      Run
        (Expression.resolve context run)
        (statements context remaining done)
    statements context (Stage1.Bind {patternx, expression} : remaining) done =
      Bind
        pattern'
        (Expression.resolve context expression)
        (statements (Pattern.augment pattern' context) remaining done)
      where
        pattern' = Pattern.resolve context patternx
    statements context (Stage1.Let declarations : remaining) done
      | (context, locals) <- Declarations.resolve context declarations =
          Let locals (statements context remaining done)
