{-# LANGUAGE_HAZY UnorderedRecords #-}
module Syntax.Tree.InstanceDeclaration where

import Syntax.Parser (Parser, position, token)
import Syntax.Position (Position)
import Syntax.Tree.LeftHandSide (LeftHandSide)
import qualified Syntax.Tree.LeftHandSide as LeftHandSide
import Syntax.Tree.RightHandSide (RightHandSide)
import qualified Syntax.Tree.RightHandSide as RightHandSide

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
