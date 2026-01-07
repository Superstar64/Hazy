-- |
-- Parser syntax tree for constructor fields
module Stage1.Tree.Field where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict
import Stage1.Parser
  ( Parser,
    sepBy1Comma,
    token,
  )
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Entry (Entry)
import qualified Stage1.Tree.Entry as Entry
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Marked as Marked
import qualified Stage1.Tree.Scheme as Scheme
import Stage1.Variable (Variable)

data Field position
  = -- |
    -- > data T = C { x :: t }
    -- >              ^^^^^^
    Field !(Strict.Vector1 (Marked Variable position)) !Entry
  deriving (Show)

instance TermBindingVariables Field where
  termBindingVariables (Field names _) = toList names

parse :: Parser (Field Position)
parse = (Field . Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral) <*> (token "::" *> Entry.parse Scheme.parse)
