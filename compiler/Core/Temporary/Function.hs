module Core.Temporary.Function where

import qualified Core.Index.Term as Term
import qualified Core.Shift as Shift2
import Core.Temporary.Pattern (Pattern)
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide)
import qualified Core.Temporary.RightHandSide as RightHandSide
import {-# SOURCE #-} qualified Core.Tree.Expression as Expression
import Core.Tree.Statements (Statements)
import qualified Core.Tree.Statements as Statements
import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Alternative as Semantic (Alternative (..))
import qualified Semantic.Tree.Function as Semantic (Function (..))
import qualified Semantic.Tree.Lambda as Semantic (Lambda)
import qualified Semantic.Tree.Lambda as Semantic.Lambda

data Function scope
  = Plain {plain :: !(RightHandSide scope)}
  | Bound
      { patternx :: !(Pattern scope),
        body :: !(Function (Scope.Pattern ':+ scope))
      }
  | Bind
      { patternx :: !(Pattern scope),
        variable :: !(Term.Index scope),
        thenx :: !(Function (Scope.Pattern ':+ scope))
      }
  deriving (Show)

instance Shift Function where
  shift = shiftDefault

instance Shift.Functor Function where
  map = Shift2.mapDefault

instance Shift2.Functor Function where
  map category = \case
    Plain {plain} ->
      Plain
        { plain = Shift2.map category plain
        }
    Bound {patternx, body} ->
      Bound
        { patternx = Shift2.map category patternx,
          body = Shift2.map (Shift2.Over category) body
        }
    Bind {patternx, variable, thenx} ->
      Bind
        { patternx = Shift2.map category patternx,
          variable = Shift2.map category variable,
          thenx = Shift2.map (Shift2.Over category) thenx
        }

class Simplify source where
  simplify :: source Normal Check scope -> Function scope

instance Simplify Semantic.Function where
  simplify = \case
    Semantic.Plain {rightHandSide} -> Plain {plain = RightHandSide.simplify rightHandSide}
    Semantic.Bound {patternx, function} ->
      Bound
        { patternx = Pattern.simplify patternx,
          body = simplify function
        }

instance Simplify Semantic.Lambda where
  simplify = \case
    Semantic.Lambda.Plain {plain} -> Plain {plain = RightHandSide.simplify plain}
    Semantic.Lambda.Bound {parameter, body} ->
      Bound
        { patternx = Pattern.simplify parameter,
          body = simplify body
        }

instance Simplify Semantic.Alternative where
  simplify Semantic.Alternative {parameter, rightHandSide} =
    Bound
      { patternx = Pattern.simplify parameter,
        body =
          Plain
            { plain = RightHandSide.simplify rightHandSide
            }
      }

etaExpandable :: Function scope -> Bool
etaExpandable Plain {} = False
etaExpandable Bound {} = True
etaExpandable Bind {thenx} = etaExpandable thenx

etaExpand :: Function scope -> Function (Scope.SimpleDeclaration ':+ scope)
etaExpand = etaExpand Shift.Shift Term.SimpleDeclaration
  where
    etaExpand :: Shift.Category scope1 scope2 -> Term.Index scope2 -> Function scope1 -> Function scope2
    etaExpand category argument = \case
      Plain {plain} ->
        Plain
          { plain =
              RightHandSide.Call
                { function = Shift.map category plain,
                  argument
                }
          }
      Bound {patternx, body} ->
        Bind
          { patternx = Shift.map category patternx,
            variable = argument,
            thenx = Shift.map (Shift.Over category) body
          }
      Bind {patternx, variable, thenx} ->
        Bind
          { patternx = Shift.map category patternx,
            variable = Shift.map category variable,
            thenx = etaExpand (Shift.Over category) (shift argument) thenx
          }

desugarUnitary :: Function scope -> Statements scope
desugarUnitary = \case
  Plain {plain} -> RightHandSide.desugar plain
  Bind {patternx, variable, thenx} ->
    Statements.bind
      patternx
      (Expression.monoVariable variable)
      (desugarUnitary thenx)
  Bound {} -> error "desugarUnitary on bound"
