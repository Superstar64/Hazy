module Semantic.Check.Temporary.TypePattern where

import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.TypePattern as Solved
import qualified Semantic.Unify as Unify
import Syntax.Lexer (VariableIdentifier)
import Syntax.Position (Position)

data TypePattern s scope = TypePattern
  { position :: !Position,
    typex :: !(Unify.Type s scope),
    name :: !VariableIdentifier
  }

solve :: TypePattern s scope -> Unify.Solve s (Solved.TypePattern Position Check scope)
solve TypePattern {position, name, typex} = do
  typex <- Unify.solve position typex
  pure
    Solved.TypePattern
      { position,
        name,
        typex = Solved typex
      }
