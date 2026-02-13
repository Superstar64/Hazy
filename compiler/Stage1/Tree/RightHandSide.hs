{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for right hand sides
module Stage1.Tree.RightHandSide where

import Stage1.Parser (Parser, asum, position, token)
import Stage1.Position (Position)
import Stage1.Tree.Body (Body)
import qualified Stage1.Tree.Body as Body
import {-# SOURCE #-} Stage1.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage1.Tree.Declarations as Declarations

data RightHandSide position
  = -- |
    -- > x = e
    -- >   ^^^
    RightHandSide
    { equalPosition :: !position,
      body :: !(Body position),
      declarations :: !(Declarations position)
    }
  deriving (Show)

parse :: Parser () -> Parser (RightHandSide Position)
parse equal =
  rightHandSide
    <$> position
    <*> Body.parse equal
    <*> asum
      [ token "where" *> Declarations.parse,
        pure Declarations.empty
      ]
  where
    rightHandSide equalPosition body declarations =
      RightHandSide
        { equalPosition,
          body,
          declarations
        }
