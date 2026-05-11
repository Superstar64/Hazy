module Stage3.Tree.Declaration where

import Stage2.Tree.Declaration (Key (..))
import Stage3.Tree.Definition4 (Definition4 (..))
import qualified Stage4.Tree.Scheme as Simple (Scheme)
import Prelude hiding (Maybe (Just))

data LazyTermDeclaration scope = !Key :^ Declaration scope
  deriving (Show)

infix 4 :^

data Declaration scope
  = Declaration
  { name :: !Key,
    definition :: !(Definition4 scope),
    typex :: !(Simple.Scheme scope)
  }
  deriving (Show)

strict :: Declaration scope -> LazyTermDeclaration scope
strict declaration@Declaration {name} = name :^ declaration

typex_ :: Declaration scope -> Simple.Scheme scope
typex_ = typex
