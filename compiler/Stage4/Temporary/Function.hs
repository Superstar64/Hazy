module Stage4.Temporary.Function where

import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Alternative as Stage3 (Alternative (..))
import qualified Stage3.Tree.Function as Stage3 (Function (..))
import qualified Stage3.Tree.Lambda as Stage3 (Lambda)
import qualified Stage3.Tree.Lambda as Stage3.Lambda
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import Stage4.Temporary.Pattern (Pattern)
import qualified Stage4.Temporary.Pattern as Pattern
import Stage4.Temporary.RightHandSide (RightHandSide)
import qualified Stage4.Temporary.RightHandSide as RightHandSide
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Expression
import Stage4.Tree.Statements (Statements)
import qualified Stage4.Tree.Statements as Statements

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
  simplify :: source scope -> Function scope

instance Simplify Stage3.Function where
  simplify = \case
    Stage3.Plain {plain} -> Plain {plain = RightHandSide.simplify plain}
    Stage3.Bound {patternx, body} ->
      Bound
        { patternx = Pattern.simplify patternx,
          body = simplify body
        }

instance Simplify Stage3.Lambda where
  simplify = \case
    Stage3.Lambda.Plain {plain} -> Plain {plain = RightHandSide.simplify plain}
    Stage3.Lambda.Bound {parameter, body} ->
      Bound
        { patternx = Pattern.simplify parameter,
          body = simplify body
        }

instance Simplify Stage3.Alternative where
  simplify Stage3.Alternative {parameter, rightHandSide} =
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
