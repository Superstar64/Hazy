module Core.Temporary.Definition where

import qualified Core.Shift as Shift2
import Core.Temporary.Function (Function)
import qualified Core.Temporary.Function as Function
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Statements as Statements
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Definition as Semantic (Definition (..))

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
  map = Shift2.mapDefault

instance Shift2.Functor Definition where
  map category = \case
    Alternative {definition, alternative} ->
      Alternative
        { definition = Shift2.map category definition,
          alternative = Shift2.map category alternative
        }
    Definition {definition} ->
      Definition
        { definition = Shift2.map category definition
        }

instance Semigroup (Definition scope) where
  Alternative {definition, alternative} <> next =
    Alternative
      { definition,
        alternative = alternative <> next
      }
  Definition {definition} <> alternative = Alternative {definition, alternative}

simplify :: Semantic.Definition Normal Check scope -> Definition scope
simplify = \case
  Semantic.Definition {definition} ->
    Definition
      { definition = Function.simplify definition
      }
  Semantic.Alternative {definition, alternative} ->
    Alternative
      { definition = Function.simplify definition,
        alternative = simplify alternative
      }

etaExpandable :: Definition scope -> Bool
etaExpandable = \case
  Definition {definition} -> Function.etaExpandable definition
  Alternative {definition, alternative} ->
    Function.etaExpandable definition || etaExpandable alternative

etaExpand :: Definition scope -> Definition (Scope.SimpleDeclaration ':+ scope)
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
