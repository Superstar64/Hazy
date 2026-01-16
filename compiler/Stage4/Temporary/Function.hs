module Stage4.Temporary.Function where

import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Alternative as Stage3 (Alternative (..))
import qualified Stage3.Tree.Function as Stage3 (Function (..))
import qualified Stage3.Tree.Lambda as Stage3 (Lambda)
import qualified Stage3.Tree.Lambda as Stage3.Lambda
import {-# SOURCE #-} qualified Stage4.Temporary.Expression as Expression
import Stage4.Temporary.Pattern (Pattern)
import qualified Stage4.Temporary.Pattern as Pattern
import Stage4.Temporary.RightHandSide (RightHandSide)
import qualified Stage4.Temporary.RightHandSide as RightHandSide
import Stage4.Temporary.Statements (Statements)
import qualified Stage4.Temporary.Statements as Statements

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
  map = Term.mapDefault

instance Term.Functor Function where
  map category = \case
    Plain {plain} ->
      Plain
        { plain = Term.map category plain
        }
    Bound {patternx, body} ->
      Bound
        { patternx = Term.map category patternx,
          body = Term.map (Term.Over category) body
        }
    Bind {patternx, variable, thenx} ->
      Bind
        { patternx = Term.map category patternx,
          variable = Term.map category variable,
          thenx = Term.map (Term.Over category) thenx
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

etaExpand :: Function scope -> Function (Scope.Declaration ':+ scope)
etaExpand = etaExpand Shift.Shift (Term.Declaration 0)
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
