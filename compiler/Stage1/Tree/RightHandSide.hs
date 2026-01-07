-- |
-- Parser syntax tree for right hand sides
module Stage1.Tree.RightHandSide where

import Stage1.Parser (Parser, asum, token)
import Stage1.Position (Position)
import Stage1.Tree.Body (Body)
import qualified Stage1.Tree.Body as Body
import {-# SOURCE #-} Stage1.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage1.Tree.Declarations as Declarations

data RightHandSide position
  = -- |
    -- > x = e
    -- >   ^^^
    RightHandSide !(Body position) !(Declarations position)
  deriving (Show)

parse :: Parser () -> Parser (RightHandSide Position)
parse equal =
  RightHandSide
    <$> Body.parse equal
    <*> asum
      [ token "where" *> Declarations.parse,
        pure Declarations.empty
      ]
