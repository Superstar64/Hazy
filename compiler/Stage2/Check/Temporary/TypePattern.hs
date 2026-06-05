module Stage2.Check.Temporary.TypePattern where

import Stage1.Lexer (VariableIdentifier)
import Stage1.Position (Position)
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.TypePattern as Solved
import qualified Stage2.Unify as Unify

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
