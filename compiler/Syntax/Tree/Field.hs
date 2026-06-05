{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for constructor fields
module Syntax.Tree.Field where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Parser
  ( Parser,
    sepBy1Comma,
    token,
  )
import Syntax.Position (Position)
import Syntax.Tree.Entry (Entry)
import qualified Syntax.Tree.Entry as Entry
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Marked as Marked
import qualified Syntax.Tree.Scheme as Scheme
import Syntax.Variable (Variable)

data Field position
  = -- |
    -- > data T = C { x :: t }
    -- >              ^^^^^^
    Field
    { names :: !(Strict.Vector1 (Marked Variable position)),
      entry :: !Entry
    }
  deriving (Show)

instance TermBindingVariables Field where
  termBindingVariables Field {names} = toList names

parse :: Parser (Field Position)
parse =
  (field . Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral)
    <*> (token "::" *> Entry.parse Scheme.parse)
  where
    field names entry = Field {names, entry}
