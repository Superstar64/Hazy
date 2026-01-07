-- |
-- Syntax tree for definition bodies
module Stage1.Tree.Body where

import qualified Data.Strict.Vector1 as Strict
import Stage1.Parser (Parser, asum, betweenBraces, some, token)
import Stage1.Position (Position)
import {-# SOURCE #-} Stage1.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage1.Tree.Expression as Expression
import {-# SOURCE #-} Stage1.Tree.Statements (Statements)
import {-# SOURCE #-} qualified Stage1.Tree.Statements as Statements

data Body position
  = -- |
    --
    --  Body without guards
    --
    --  > x = e
    --  >   ^^^
    --
    --  > case e of { x -> e }
    --  >               ^^^^
    Body !(Expression position)
  | -- |
    --
    --  Body with guards
    --
    --  > x | x <- e = e
    --  >   ^^^^^^^^^^^^
    --
    --  > case e of { x | x <- e -> e }
    --  >               ^^^^^^^^^^^^^
    --
    -- > x of x <- e; e
    -- >   ^^^^^^^^^^^^
    Guards !(Strict.Vector1 (Statements position))
  deriving (Show)

parse ::
  -- | Delimiter token
  Parser () ->
  Parser (Body Position)
parse equal =
  asum
    [ Body <$> (equal *> Expression.parse),
      Guards . Strict.fromNonEmpty <$> some guards
    ]
  where
    guards =
      asum
        [ token "|" *> Statements.parseComprehension <*> (equal *> Expression.parse),
          token "of" *> betweenBraces Statements.parseDo
        ]
