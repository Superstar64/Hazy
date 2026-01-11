{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for import qualifications
module Stage1.Tree.Qualification where

import Stage1.Parser
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
