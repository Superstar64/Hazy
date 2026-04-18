module Stage2.Group.Tree.TermDeclaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import Stage2.Group.Tree.Group (Group)
import Stage2.Tree.Definition2 (Annotated, Definition2)
import Stage2.Tree.Scheme (Scheme)

data TermDeclaration scope
  = Annotated
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity,
        annotation :: !(Scheme Position scope),
        definition :: !(Definition2 Annotated scope)
      }
  | Group !(Group scope)
  deriving (Show)
