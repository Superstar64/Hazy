{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Constructor where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Entry (Entry)
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)
import Syntax.Variable (Variable)
import qualified Syntax.Variable as Variable (Constructor)

data Syntax
  = Standard
  | Infix !Fixity
  | Record !(Strict.Vector Variable)
  deriving (Show)

data Constructor stage scope
  = Constructor
  { position :: !Position,
    name :: !Variable.Constructor,
    syntax :: !Syntax,
    entries :: !(Strict.Vector (Entry Position stage scope))
  }
  deriving (Show)

instance Shift0.Functor (Constructor stage) where
  map = Shift.mapDefault

instance Shift.Functor (Constructor stage) where
  map category = \case
    Constructor {position, name, syntax, entries} ->
      Constructor
        { position,
          name,
          syntax,
          entries = fmap (Shift.map category) entries
        }

instance FreeTypeVariables Constructor where
  freeTypeVariables target (Constructor {entries}) =
    foldMap (freeTypeVariables target) entries
