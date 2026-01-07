-- |
-- Parser syntax tree for gadt constructors
module Stage1.Tree.GADTConstructor where

import qualified Data.Strict.Vector1 as Strict
import Stage1.Parser
  ( Parser,
    sepBy1Comma,
    token,
  )
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked)
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.Scheme (Scheme)
import qualified Stage1.Tree.Scheme as Scheme
import Stage1.Variable (Constructor)

data GADTConstructor
  = -- |
    -- > data T where { C :: T }
    -- >                ^^^^^^
    GADTConstructor !(Strict.Vector1 (Marked Constructor Position)) !(Scheme Position)
  deriving (Show)

parse :: Parser GADTConstructor
parse = GADTConstructor . Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral <*> (token "::" *> Scheme.parse)
