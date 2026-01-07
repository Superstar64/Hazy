module Stage4.Temporary.RightHandSide where

import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Body as Stage3 (Body (Body))
import qualified Stage3.Tree.Body as Stage3.Body
import qualified Stage3.Tree.RightHandSide as Stage3 (RightHandSide (..))
import {-# SOURCE #-} Stage4.Temporary.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage4.Temporary.Declarations as Declarations
import {-# SOURCE #-} qualified Stage4.Temporary.Expression as Expression
import Stage4.Temporary.Statements (Statements)
import qualified Stage4.Temporary.Statements as Statements

data RightHandSide scope
  = RightHandSide
      { letBody :: !(Statements (Scope.Declaration ':+ scope)),
        declarations :: !(Declarations (Scope.Declaration ':+ scope))
      }
  | Call
      { function :: !(RightHandSide scope),
        argument :: !(Term.Index scope)
      }
  deriving (Show)

instance Shift RightHandSide where
  shift = shiftDefault

instance Shift.Functor RightHandSide where
  map = Term.mapDefault

instance Term.Functor RightHandSide where
  map category = \case
    RightHandSide {letBody, declarations} ->
      RightHandSide
        { letBody = Term.map (Term.over category) letBody,
          declarations = Term.map (Term.over category) declarations
        }
    Call {function, argument} ->
      Call
        { function = Term.map category function,
          argument = Term.map category argument
        }

simplify :: Stage3.RightHandSide scope -> RightHandSide scope
simplify Stage3.RightHandSide {Stage3.body, Stage3.declarations}
  | letBody <- case body of
      Stage3.Body {Stage3.Body.body} ->
        Statements.Done {Statements.done = Expression.simplify body}
      Stage3.Body.Guards {Stage3.Body.guards} ->
        foldr1 Statements.Branch (Statements.simplify <$> guards) =
      RightHandSide
        { letBody,
          declarations = Declarations.simplify declarations
        }

desugar :: RightHandSide scope -> Statements scope
desugar = \case
  RightHandSide {letBody, declarations} ->
    Statements.Let
      { Statements.letBody,
        Statements.declarations
      }
  Call {function, argument} ->
    Statements.call (desugar function) (Expression.monoVariable argument)
