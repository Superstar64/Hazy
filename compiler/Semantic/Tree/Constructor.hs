{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Constructor where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (freeTypeVariables))
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Entry (Entry)
import Semantic.Tree.Field (Field)
import Syntax.Position (Position)
import qualified Syntax.Variable as Variable (Constructor)

data Constructor stage scope
  = Constructor
      { position :: !Position,
        name :: !Variable.Constructor,
        entries :: !(Strict.Vector (Entry Position stage scope))
      }
  | Record
      { position :: !Position,
        name :: !Variable.Constructor,
        fields :: !(Strict.Vector (Field stage scope))
      }
  deriving (Show)

instance Shift (Constructor stage) where
  shift = shiftDefault

instance Shift.Functor (Constructor stage) where
  map category = \case
    Constructor {position, name, entries} ->
      Constructor
        { position,
          name,
          entries = fmap (Shift.map category) entries
        }
    Record {position, name, fields} ->
      Record
        { position,
          name,
          fields = fmap (Shift.map category) fields
        }

instance FreeTypeVariables Constructor where
  freeTypeVariables target = \case
    Constructor {entries} -> foldMap (freeTypeVariables target) entries
    Record {fields} -> foldMap (freeTypeVariables target) fields
