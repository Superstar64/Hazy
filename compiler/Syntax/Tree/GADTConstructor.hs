{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for gadt constructors
module Syntax.Tree.GADTConstructor where

import qualified Data.Strict.Vector1 as Strict
import Syntax.Parser
  ( Parser,
    sepBy1Comma,
    token,
  )
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked)
import qualified Syntax.Tree.Marked as Marked
import Syntax.Tree.Scheme (Scheme)
import qualified Syntax.Tree.Scheme as Scheme
import Syntax.Variable (Constructor)

data GADTConstructor
  = -- |
    -- > data T where { C :: T }
    -- >                ^^^^^^
    GADTConstructor
    { names :: !(Strict.Vector1 (Marked Constructor Position)),
      scheme :: !(Scheme Position)
    }
  deriving (Show)

parse :: Parser GADTConstructor
parse =
  gadtConstructor . Strict.fromNonEmpty
    <$> sepBy1Comma Marked.parseLiteral
    <*> (token "::" *> Scheme.parse)
  where
    gadtConstructor names scheme = GADTConstructor {names, scheme}
