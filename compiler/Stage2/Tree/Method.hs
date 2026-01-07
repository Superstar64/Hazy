{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Method where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition (Definition)
import Stage2.Tree.Scheme (Scheme)

data Method scope = Method
  { position :: !Position,
    name :: !Variable,
    annotation :: !(Scheme Position scope),
    definition :: !(Strict.Maybe (Definition scope))
  }
  deriving (Show)

instance Shift Method where
  shift = shiftDefault

instance Shift.Functor Method where
  map category Method {position, name, annotation, definition} =
    Method
      { position,
        name,
        annotation = Shift.map category annotation,
        definition = fmap (Shift.map category) definition
      }
