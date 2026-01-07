{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for constructors
module Stage1.Tree.Constructor where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenBraces,
    many,
    sepByComma,
    token,
    try,
    (<**>),
  )
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Entry (Entry)
import qualified Stage1.Tree.Entry as Entry
import Stage1.Tree.Field (Field)
import qualified Stage1.Tree.Field as Field
import qualified Stage1.Tree.Marked as Marked
import qualified Stage1.Tree.Scheme as Scheme
import qualified Stage1.Tree.Type as Type

data Constructor position
  = -- |
    --  > data T = A B
    --  >          ^^^
    Constructor
      { constructor :: !(Marked.Constructor position),
        entries :: !(Strict.Vector Entry)
      }
  | -- |
    --  > data T = A :+ B
    --  >          ^^^^^^
    Infix
      { left :: !Entry,
        constructor :: !(Marked.Constructor position),
        right :: !Entry
      }
  | -- |
    --  > data T = A { x :: B }
    --  >          ^^^^^^^^^^^^
    Record
      { constructor :: !(Marked.Constructor position),
        fields :: !(Strict.Vector (Field position))
      }
  deriving (Show)

instance TermBindingVariables Constructor where
  termBindingVariables = \case
    Constructor {} -> []
    Infix {} -> []
    Record {fields} -> foldMap termBindingVariables fields

parseMany :: Parser (Strict.Vector (Constructor Position))
parseMany =
  asum
    [ Strict.Vector.fromList <$> ((:) <$> (token "=" *> parse) <*> many (token "|" *> parse)),
      pure Strict.Vector.empty
    ]

parse :: Parser (Constructor Position)
parse =
  asum
    [ try (infixx <$> Entry.parse (Scheme.mono <$> Type.parse2) <*> Marked.parseOperator)
        <*> Entry.parse (Scheme.mono <$> Type.parse2),
      Marked.parseLiteral
        <**> asum
          [ record <$> betweenBraces (Strict.Vector.fromList <$> sepByComma Field.parse),
            constructor . Strict.Vector.fromList <$> many (Entry.parse (Scheme.mono <$> Type.parse3))
          ]
    ]
  where
    infixx left constructor right = Infix {left, constructor, right}
    constructor entries constructor = Constructor {constructor, entries}
    record fields constructor = Record {constructor, fields}
