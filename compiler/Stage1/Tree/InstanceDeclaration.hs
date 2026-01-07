module Stage1.Tree.InstanceDeclaration where

import Stage1.Parser (Parser, position, token)
import Stage1.Position (Position)
import Stage1.Tree.LeftHandSide (LeftHandSide)
import qualified Stage1.Tree.LeftHandSide as LeftHandSide
import Stage1.Tree.RightHandSide (RightHandSide)
import qualified Stage1.Tree.RightHandSide as RightHandSide

data InstanceDeclaration position
  = -- |
    --  > x = e
    Definition
    { startPosition :: !position,
      leftHandSide :: !(LeftHandSide position),
      rightHandSide :: !(RightHandSide position)
    }
  deriving (Show)

parse :: Parser (InstanceDeclaration Position)
parse =
  definition <$> position <*> LeftHandSide.parse <*> RightHandSide.parse (token "=")
  where
    definition startPosition leftHandSide rightHandSide =
      Definition
        { startPosition,
          leftHandSide,
          rightHandSide
        }
