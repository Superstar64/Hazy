module Stage3.Tree.Declaration where

import Stage2.Tree.Declaration (Key (..))
import qualified Stage3.Tree.Definition2 as Definition2
import Stage3.Tree.Definition3 (Definition3 (..))
import Stage3.Tree.Definition4 (Definition4 (..))
import qualified Stage4.Tree.Scheme as Simple (Scheme (Scheme))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import Prelude hiding (Maybe (Just))

data LazyTermDeclaration scope = !Key :^ Declaration scope
  deriving (Show)

infix 4 :^

data Declaration scope
  = Declaration
  { name :: !Key,
    definition :: !(Definition4 scope)
  }
  deriving (Show)

strict :: Declaration scope -> LazyTermDeclaration scope
strict declaration@Declaration {name} = name :^ declaration

simple :: Declaration scope -> Simple.Scheme scope
simple = \case
  Declaration {definition} -> case definition of
    _ ::: _ ::@ Simple.SchemeOver {parameters, constraints, result}
      | result <- Definition2.typex result ->
          Simple.Scheme
            Simple.SchemeOver
              { parameters,
                constraints,
                result
              }
