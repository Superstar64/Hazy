module Stage4.Temporary.RightHandSide where

import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Body as Stage3 (Body (Body))
import qualified Stage3.Tree.Body as Stage3.Body
import qualified Stage3.Tree.Expression as Stage3 (Expression)
import qualified Stage3.Tree.RightHandSide as Stage3 (RightHandSide (..))
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import {-# SOURCE #-} Stage4.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Tree.Declarations as Declarations
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Expression
import Stage4.Tree.Statements (Statements)
import qualified Stage4.Tree.Statements as Statements

data RightHandSide scope
  = RightHandSide
      { letBody :: !(Statements (Scope.Declaration ':+ scope)),
        declarations :: !(Declarations (Scope.Declaration ':+ scope))
      }
  | Call
      { function :: !(RightHandSide scope),
        argument :: !(Term.Index scope)
      }
  | Done {done :: !(Expression scope)}
  deriving (Show)

instance Shift RightHandSide where
  shift = shiftDefault

instance Shift.Functor RightHandSide where
  map = Shift2.mapDefault

instance Shift2.Functor RightHandSide where
  map category = \case
    RightHandSide {letBody, declarations} ->
      RightHandSide
        { letBody = Shift2.map (Shift2.Over category) letBody,
          declarations = Shift2.map (Shift2.Over category) declarations
        }
    Call {function, argument} ->
      Call
        { function = Shift2.map category function,
          argument = Shift2.map category argument
        }
    Done {done} -> Done {done = Shift2.map category done}

class Simplify source where
  simplify :: source scope -> RightHandSide scope

instance Simplify Stage3.RightHandSide where
  simplify Stage3.RightHandSide {body, declarations}
    | letBody <- case body of
        Stage3.Body {body} ->
          Statements.Done {done = Expression.simplify body}
        Stage3.Body.Guards {guards} ->
          foldr1 Statements.Branch (Statements.simplify <$> guards) =
        RightHandSide
          { letBody,
            declarations = Declarations.simplify declarations
          }

instance Simplify Stage3.Expression where
  simplify done = Done {done = Expression.simplify done}

desugar :: RightHandSide scope -> Statements scope
desugar = \case
  RightHandSide {letBody, declarations} ->
    Statements.Let
      { letBody,
        declarations
      }
  Call {function, argument} ->
    Statements.call (desugar function) (Expression.monoVariable argument)
  Done {done} -> Statements.Done {done}
