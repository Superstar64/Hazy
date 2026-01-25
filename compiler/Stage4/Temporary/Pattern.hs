module Stage4.Temporary.Pattern where

import qualified Data.Strict.Vector1 as Strict (Vector1)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Constructor as Constructor
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Pattern as Stage3
import qualified Stage3.Tree.PatternField as Stage3.Field
import qualified Stage4.Shift as Shift2

data Pattern scope
  = Wildcard
  | Match {match :: !(Bindings scope), irrefutable :: !Prelude.Bool}
  deriving (Show)

data Bindings scope
  = Constructor
      { constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern scope))
      }
  | Record
      { constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope)),
        fieldCount :: !Int
      }
  | List {items :: !(Strict.Vector1 (Pattern scope))}
  | Character {character :: !Char}
  | String {text :: !Text}
  deriving (Show)

instance Shift Pattern where
  shift = shiftDefault

instance Shift.Functor Pattern where
  map = Shift2.mapDefault

instance Shift2.Functor Pattern where
  map category = \case
    Wildcard -> Wildcard
    Match {match, irrefutable} ->
      Match
        { match = Shift2.map category match,
          irrefutable
        }

instance Shift Bindings where
  shift = shiftDefault

instance Shift.Functor Bindings where
  map = Shift2.mapDefault

instance Shift2.Functor Bindings where
  map category = \case
    Constructor {constructor, patterns} ->
      Constructor
        { constructor = Shift2.map category constructor,
          patterns = Shift2.map category <$> patterns
        }
    Record {constructor, fields, fieldCount} ->
      Record
        { constructor = Shift2.map category constructor,
          fields = Shift2.map category <$> fields,
          fieldCount
        }
    List {items} ->
      List
        { items = Shift2.map category <$> items
        }
    Character {character} -> Character {character}
    String {text} -> String {text}

instance Shift Field where
  shift = shiftDefault

instance Shift.Functor Field where
  map = Shift2.mapDefault

instance Shift2.Functor Field where
  map category (Field index patternx) = Field index (Shift2.map category patternx)

data Field scope = Field !Int !(Pattern scope)
  deriving (Show)

simplify :: Stage3.Pattern scope -> Pattern scope
simplify (Stage3.At match) = case match of
  Stage3.Wildcard -> Wildcard
  Stage3.Match {match, irrefutable} ->
    Match
      { match = simplifyBindings match,
        irrefutable
      }

simplifyBindings :: Stage3.Bindings scope -> Bindings scope
simplifyBindings = \case
  Stage3.Constructor {constructor, patterns} ->
    Constructor
      { constructor,
        patterns = simplify <$> patterns
      }
  Stage3.Record {constructor, fields, fieldCount} ->
    Record
      { constructor,
        fields = resolveField <$> fields,
        fieldCount
      }
  Stage3.List {items} ->
    List
      { items = simplify <$> items
      }
  Stage3.Character {character} -> Character {character}
  Stage3.String {text} -> String {text}

resolveField :: Stage3.Field.Field scope -> Field scope
resolveField (Stage3.Field.Field index patternx) = Field index (simplify patternx)
