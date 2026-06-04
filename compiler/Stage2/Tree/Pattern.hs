{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Pattern where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Vector1 as Strict (Vector1)
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import qualified Stage2.Index.Constructor as Constructor (Index (..))
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.PatternField (Field (..))
import qualified Stage2.Tree.PatternField as Field (neverFails)
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import qualified Stage4.Tree.Evidence as Simple
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)
import qualified Prelude

data Pattern stage scope
  = Wildcard
      { names :: !(Map Variable Position)
      }
  | Constructor
      { constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern stage scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Record
      { constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field stage scope)),
        single :: !Prelude.Bool,
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Integer
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        integer :: !Integer,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Float
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        float :: !Rational,
        evidence :: !(Inferred Simple.Evidence stage scope),
        equal :: !(Inferred Simple.Evidence stage scope)
      }
  | Character
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        string :: !Text
      }
  | List
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        items :: !(Strict.Vector1 (Pattern stage scope))
      }
  deriving (Show)

instance Shift (Pattern stage) where
  shift = shiftDefault

instance Shift.Functor (Pattern stage) where
  map category = \case
    Wildcard {names} -> Wildcard {names}
    Constructor {names, irrefutable, constructorPosition, constructor, patterns, single, constructorInfo} ->
      Constructor
        { names,
          irrefutable,
          constructorPosition,
          constructor = Shift.map category constructor,
          patterns = fmap (Shift.map category) patterns,
          single,
          constructorInfo = Shift.map category constructorInfo
        }
    Record {names, irrefutable, constructorPosition, constructor, fields, single, constructorInfo} ->
      Record
        { names,
          irrefutable,
          constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields,
          single,
          constructorInfo = Shift.map category constructorInfo
        }
    Integer {names, irrefutable, startPosition, integer, evidence, equal} ->
      Integer
        { names,
          irrefutable,
          startPosition,
          integer,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Float {names, irrefutable, startPosition, float, evidence, equal} ->
      Float
        { names,
          irrefutable,
          startPosition,
          float,
          evidence = Shift.map category evidence,
          equal = Shift.map category equal
        }
    Character {names, irrefutable, startPosition, character} ->
      Character {names, irrefutable, startPosition, character}
    String {names, irrefutable, startPosition, string} ->
      String
        { names,
          irrefutable,
          startPosition,
          string
        }
    List {names, irrefutable, startPosition, items} ->
      List
        { names,
          irrefutable,
          startPosition,
          items = fmap (Shift.map category) items
        }

lazy :: Pattern stage scope -> Pattern stage scope
lazy = \case
  Wildcard {names} -> Wildcard {names}
  Constructor {names, constructorPosition, constructor, patterns, single, constructorInfo} ->
    Constructor {names, irrefutable, constructorPosition, constructor, patterns, single, constructorInfo}
  Record {names, constructorPosition, constructor, fields, single, constructorInfo} ->
    Record {names, irrefutable, constructorPosition, constructor, fields, single, constructorInfo}
  Integer {names, startPosition, integer, evidence, equal} ->
    Integer {names, irrefutable, startPosition, integer, evidence, equal}
  Float {names, startPosition, float, evidence, equal} ->
    Float {names, irrefutable, startPosition, float, evidence, equal}
  Character {names, startPosition, character} -> Character {names, irrefutable, startPosition, character}
  String {names, startPosition, string} -> String {names, irrefutable, startPosition, string}
  List {names, startPosition, items} -> List {names, irrefutable, startPosition, items}
  where
    irrefutable = Prelude.True

neverFails :: Pattern stage scope -> Prelude.Bool
neverFails = \case
  Wildcard {} -> Prelude.True
  patternx | irrefutable patternx -> Prelude.True
  Constructor {single, patterns} -> single && all neverFails patterns
  Record {single, fields} -> single && all Field.neverFails fields
  Integer {} -> Prelude.False
  Float {} -> Prelude.False
  Character {} -> Prelude.False
  String {} -> Prelude.False
  List {} -> Prelude.False

variable :: Position -> Variable -> Pattern stage scope
variable position name = Wildcard {names = Map.singleton name position}
