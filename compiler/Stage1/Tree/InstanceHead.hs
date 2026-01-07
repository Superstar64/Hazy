{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for instance heads
module Stage1.Tree.InstanceHead where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenBrackets,
    betweenParens,
    many,
    position,
    sepByComma,
    some,
    token,
    try,
  )
import Stage1.Position (Position)
import Stage1.Tree.TypePattern (TypePattern)
import qualified Stage1.Tree.TypePattern as TypePattern
import Stage1.Variable (QualifiedConstructor, QualifiedConstructorIdentifier)
import qualified Stage1.Variable as Variable

data InstanceHead
  = -- |
    -- > instance C T
    -- >            ^
    Head
      { startPosition :: !Position,
        typeName :: !QualifiedConstructorIdentifier,
        parameters :: !(Strict.Vector (TypePattern Position))
      }
  | -- |
    -- > instance C 'T
    -- >            ^^
    Lifted
      { startPosition :: !Position,
        constructorName :: !QualifiedConstructor,
        parameters :: !(Strict.Vector (TypePattern Position))
      }
  | -- |
    -- > instance C (,,)
    -- >            ^^^^
    TupleN
      { count :: !Int,
        parameters :: !(Strict.Vector (TypePattern Position))
      }
  | -- |
    -- > instance C []
    -- >            ^^
    List0
      {
      }
  | -- |
    -- > instance C ([] a)
    -- >            ^^^^^^
    List1
      { parameter :: !(TypePattern Position)
      }
  | -- |
    -- > instance C (->)
    -- >            ^^^^
    Arrow0
      {
      }
  | -- |
    -- > instance C ((->) a)
    -- >            ^^^^^^^^
    Arrow1
      { parameter :: !(TypePattern Position)
      }
  | -- |
    -- > instance C ((->) a b)
    -- >            ^^^^^^^^^^
    Arrow2
      { parameter :: !(TypePattern Position),
        parameter' :: !(TypePattern Position)
      }
  | -- |
    -- > instance C (a, b)
    -- >            ^^^^^^
    Tuple
      { parameters :: !(Strict.Vector (TypePattern Position))
      }
  | -- |
    -- > instance C [a]
    -- >            ^^^
    List
      { parameter :: !(TypePattern Position)
      }
  | -- |
    -- > instance C (a -> b)
    -- >            ^^^^^^^^
    Arrow
      { parameter :: !(TypePattern Position),
        parameter' :: !(TypePattern Position)
      }
  deriving (Show)

parse :: Parser InstanceHead
parse =
  asum
    [ head <$> position <*> Variable.parse <*> pure Strict.Vector.empty,
      lifted <$> position <*> try (token "'" *> Variable.parseLiteral) <*> pure Strict.Vector.empty,
      tupleN <$> try (betweenParens $ length <$> some (token ",")) <*> pure Strict.Vector.empty,
      list0 <$ try (betweenBrackets (pure ())),
      arrow0 <$ try (betweenParens (token "->")),
      tuple <$> try (betweenParens (Strict.Vector.fromList <$> sepByComma TypePattern.parse)),
      list <$> betweenBrackets TypePattern.parse,
      betweenParens instanceHeadFull
    ]
  where
    head startPosition typeName parameters =
      Head
        { startPosition,
          typeName,
          parameters
        }
    lifted startPosition constructorName parameters =
      Lifted
        { startPosition,
          constructorName,
          parameters
        }
    tupleN count parameters =
      TupleN
        { count,
          parameters
        }
    list0 = List0 {}
    list1 parameter =
      List1
        { parameter
        }
    arrow0 =
      Arrow0
        {
        }
    arrow1 parameter =
      Arrow1
        { parameter
        }
    arrow2 parameter parameter' =
      Arrow2
        { parameter,
          parameter'
        }
    tuple parameters =
      Tuple
        { parameters
        }
    list parameter =
      List
        { parameter
        }
    arrow parameter parameter' =
      Arrow
        { parameter,
          parameter'
        }
    instanceHeadFull :: Parser InstanceHead
    instanceHeadFull =
      asum
        [ head <$> position <*> Variable.parse <*> parameters,
          lifted <$> position <*> try (token "'" *> Variable.parseLiteral) <*> parameters,
          tupleN <$> try (betweenParens $ length <$> some (token ",")) <*> parameters,
          try $ list1 <$ betweenBrackets (pure ()) <*> TypePattern.parse,
          list0 <$ betweenBrackets (pure ()),
          try $ arrow2 <$ betweenParens (token "->") <*> TypePattern.parse <*> TypePattern.parse,
          try $ arrow1 <$ betweenParens (token "->") <*> TypePattern.parse,
          arrow0 <$ try (betweenParens (token "->")),
          arrow <$> TypePattern.parse <* token "->" <*> TypePattern.parse
        ]
      where
        parameters = Strict.Vector.fromList <$> many TypePattern.parse
