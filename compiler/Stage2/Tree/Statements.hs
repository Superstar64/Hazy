module Stage2.Tree.Statements where

import Data.Foldable (toList)
import Stage1.Position (Position)
import qualified Stage1.Tree.Expression as Stage1 (Expression (..))
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
resolve context (Stage1.Statements statements1 expression1) = statements context (toList statements1) expression1
  where
    statements :: Context scope -> [Stage1.Statement Position] -> Stage1.Expression Position -> Statements scope
    statements context [] expression1 = Done (Expression.resolve context expression1)
    statements context (Stage1.Run expression1 : expressions) done =
      Run
        (Expression.resolve context expression1)
        (statements context expressions done)
    statements context (Stage1.Bind pattern1 expression1 : expressions) done =
      Bind
        pattern'
        (Expression.resolve context expression1)
        (statements (Pattern.augment pattern' context) expressions done)
      where
        pattern' = Pattern.resolve context pattern1
    statements context (Stage1.Let2 declarations : expressions) done
      | (context, locals) <- Declarations.resolve context declarations =
          Let locals (statements context expressions done)
