module Core.Temporary.RightHandSide where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import {-# SOURCE #-} Core.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Core.Tree.Declarations as Declarations
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Core.Tree.Expression as Expression
import Core.Tree.Statements (Statements)
import qualified Core.Tree.Statements as Statements
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Body as Semantic (Body (Body))
import qualified Semantic.Tree.Body as Semantic.Body
import qualified Semantic.Tree.Expression as Semantic (Expression)
import qualified Semantic.Tree.RightHandSide as Semantic (RightHandSide (..))

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
  simplify :: source Normal Check scope -> RightHandSide scope

instance Simplify Semantic.RightHandSide where
  simplify Semantic.RightHandSide {body, declarations}
    | letBody <- case body of
        Semantic.Body {body} ->
          Statements.Done {done = Expression.simplify body}
        Semantic.Body.Guards {guards} ->
          foldr1 Statements.Branch (Statements.simplify <$> guards) =
        RightHandSide
          { letBody,
            declarations = Declarations.simplify declarations
          }

instance Simplify Semantic.Expression where
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
