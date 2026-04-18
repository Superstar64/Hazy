module Stage2.Group.Tree.Definition3 where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import Stage2.Tree.Definition2 (Definition2, Inferred)
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)

data Definition3 scope
  = Inferred
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        definition' :: !(Definition2 Inferred scope)
      }
  | Shared
      { equalPosition :: !Position,
        patternx :: !(Pattern scope),
        definition :: !(RightHandSide scope)
      }
  deriving (Show)
