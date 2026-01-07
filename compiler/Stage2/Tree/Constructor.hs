{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Constructor where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import qualified Stage1.Variable as Variable (Constructor)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Entry (Entry)
import Stage2.Tree.Field (Field)

data Constructor scope
  = Constructor
      { position :: !Position,
        name :: !Variable.Constructor,
        entries :: !(Strict.Vector (Entry Position scope))
      }
  | Record
      { position :: !Position,
        name :: !Variable.Constructor,
        fields :: !(Strict.Vector (Field scope))
      }
  deriving (Show)

instance Shift Constructor where
  shift = shiftDefault

instance Shift.Functor Constructor where
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
