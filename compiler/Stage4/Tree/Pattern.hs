module Stage4.Tree.Pattern where

import qualified Stage2.Index.Constructor as Constructor
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute

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
  map = Shift2.mapDefault

instance Shift2.Functor Pattern where
  map = Substitute.mapDefault

instance Substitute.Functor Pattern where
  map category = \case
    Constructor {constructor, patterns} ->
      Constructor
        { constructor = Substitute.map category constructor,
          patterns
        }
    Character {character} -> Character {character}
