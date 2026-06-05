{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for import qualifications
module Syntax.Tree.Qualification where

import Syntax.Parser
  ( Parser,
    optional,
    token,
  )

data Qualification
  = -- |
    -- > import qualified M
    -- >        ^^^^^^^^^
    Qualified
  | Unqualified
  deriving (Show)

parse :: Parser Qualification
parse = qualification <$> optional (token "qualified")
  where
    qualification = \case
      Nothing -> Unqualified
      Just () -> Qualified
