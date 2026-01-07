module Stage4.Tree.Pattern where

import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Term as Term
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

data Pattern scope
  = Constructor
      { constructor :: !(Constructor.Index scope),
        patterns :: !Int
      }
  | Character {character :: !Char}
  deriving (Show)

instance Shift Pattern where
  shift = shiftDefault

instance Shift.Functor Pattern where
  map category = \case
    Constructor {constructor, patterns} ->
      Constructor
        { constructor = Shift.map category constructor,
          patterns
        }
    Character {character} -> Character {character}

instance Term.Functor Pattern where
  map Term.Category {Term.general} = Shift.map general
