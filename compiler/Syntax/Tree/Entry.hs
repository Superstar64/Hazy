{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for constructor entries
module Syntax.Tree.Entry where

import Syntax.Parser
  ( Parser,
    asum,
    betweenParens,
    position,
    token,
    try,
  )
import Syntax.Position (Position)
import Syntax.Tree.Scheme (Scheme)
import qualified Syntax.Tree.Scheme as Scheme
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type

data Entry
  = -- |
    -- > data T = A B
    -- >            ^
    Lazy
      { startPosition :: !Position,
        entry :: !(Scheme Position)
      }
  | -- |
    -- > data T = A !B
    -- >            ^^
    Strict
      { startPosition :: !Position,
        entry :: !(Scheme Position)
      }
  | Polymorphic
      { startPosition :: !Position,
        levity :: !(Type Position),
        entry :: !(Scheme Position)
      }
  deriving (Show)

parse :: Parser (Scheme Position) -> Parser Entry
parse scheme =
  asum
    [ try $ lazy <$> scheme,
      lazy <$> betweenParens Scheme.parse,
      strict
        <$> (position <* token "!")
        <*> asum
          [ betweenParens Scheme.parse,
            Scheme.mono <$> Type.parse3
          ],
      polymorphic
        <$> (position <* token "~!")
        <*> Type.parse3
        <*> asum
          [ betweenParens Scheme.parse,
            Scheme.mono <$> Type.parse3
          ]
    ]
  where
    lazy entry =
      Lazy
        { startPosition = Scheme.startPosition entry,
          entry
        }
    strict startPosition entry =
      Strict
        { startPosition,
          entry
        }
    polymorphic startPosition levity entry =
      Polymorphic
        { startPosition,
          levity,
          entry
        }
