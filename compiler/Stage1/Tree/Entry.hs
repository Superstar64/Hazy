{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for constructor entries
module Stage1.Tree.Entry where

import Stage1.Parser
  ( Parser,
    asum,
    betweenParens,
    position,
    token,
  )
import Stage1.Position (Position)
import Stage1.Tree.Scheme (Scheme)
import qualified Stage1.Tree.Scheme as Scheme
import qualified Stage1.Tree.Type as Type

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
  deriving (Show)

parse :: Parser (Scheme Position) -> Parser Entry
parse scheme =
  asum
    [ lazy <$> betweenParens Scheme.parse,
      lazy <$> scheme,
      strict
        <$> (position <* token "!")
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
