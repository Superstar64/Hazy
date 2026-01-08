module Stage4.Temporary.Definition where

import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Definition as Stage3 (Definition (..))
import Stage4.Temporary.Expression (Expression)
import qualified Stage4.Temporary.Expression as Expression
import Stage4.Temporary.Function (Function)
import qualified Stage4.Temporary.Function as Function
import qualified Stage4.Temporary.Statements as Statements

data Definition scope
  = Alternative
      { definition :: !(Function scope),
        alternative :: !(Definition scope)
      }
  | Definition
      { definition :: !(Function scope)
      }
  deriving (Show)

instance Shift Definition where
  shift = shiftDefault

instance Shift.Functor Definition where
  map = Term.mapDefault

instance Term.Functor Definition where
  map category = \case
    Alternative {definition, alternative} ->
      Alternative
        { definition = Term.map category definition,
          alternative = Term.map category alternative
        }
    Definition {definition} ->
      Definition
        { definition = Term.map category definition
        }

instance Semigroup (Definition scope) where
  Alternative {definition, alternative} <> next =
    Alternative
      { definition,
        alternative = alternative <> next
      }
  Definition {definition} <> alternative = Alternative {definition, alternative}

simplify :: Stage3.Definition scope -> Definition scope
simplify = \case
  Stage3.Definition {definition} ->
    Definition
      { definition = Function.simplify definition
      }
  Stage3.Alternative {definition, alternative} ->
    Alternative
      { definition = Function.simplify definition,
        alternative = simplify alternative
      }

etaExpandable :: Definition scope -> Bool
etaExpandable = \case
  Definition {definition} -> Function.etaExpandable definition
  Alternative {definition, alternative} ->
    Function.etaExpandable definition || etaExpandable alternative

etaExpand :: Definition scope -> Definition (Scope.Declaration ':+ scope)
etaExpand = \case
  Definition {definition} -> Definition {definition = Function.etaExpand definition}
  Alternative {definition, alternative} ->
    Alternative
      { definition = Function.etaExpand definition,
        alternative = etaExpand alternative
      }

desugar :: Definition scope -> Expression scope
desugar cases
  | etaExpandable cases =
      Expression.Lambda
        { body = desugar $ etaExpand cases
        }
  | otherwise =
      Expression.Join
        { statements = go cases
        }
  where
    go Definition {definition} = Function.desugarUnitary definition
    go Alternative {definition, alternative} =
      Statements.Branch
        { left = Function.desugarUnitary definition,
          right = go alternative
        }
