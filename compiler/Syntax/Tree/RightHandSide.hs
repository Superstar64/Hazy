{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for right hand sides
module Syntax.Tree.RightHandSide where

import Syntax.Parser (Parser, asum, position, token)
import Syntax.Position (Position)
import Syntax.Tree.Body (Body)
import qualified Syntax.Tree.Body as Body
import {-# SOURCE #-} Syntax.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Syntax.Tree.Declarations as Declarations

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
