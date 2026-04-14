{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Method where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.FreeVariables (FreeTypeVariables (freeTypeVariables))
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

instance FreeTypeVariables Method where
  -- This intentionally doesn't take the free type variables of the type class
  -- defaults. At the moment Stage2 free type variables will only be used for
  -- computing strongly connected components for kind checking type
  -- declarations and type class default methods should not be a part of that.

  -- At some point in the future there should be a seperate TypeDeclarationExtra
  -- like how Stage3 has it. That way, `FreeTypeVariables TypeDeclarationExtra`
  -- can simplify not be implemented.
  freeTypeVariables target Method {annotation} = freeTypeVariables target annotation
