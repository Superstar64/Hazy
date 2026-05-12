{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Pattern where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector1 as Strict.Vector1
import qualified Data.Strict.Vector2 as Strict.Vector2
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateVariableEntries)
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (..))
import qualified Stage1.Tree.PatternInfix as Stage1.Infix
import Stage1.Variable (Variable)
import qualified Stage2.Index.Constructor as Constructor (Index (..), cons, nil, tuple)
import qualified Stage2.Index.Term as Term (Bound (..), Index (..))
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Resolve.Binding.Term as Term (Binding (..), Selector (..))
import Stage2.Resolve.Bindings (constructors, stability, terms, types, (</>))
import qualified Stage2.Resolve.Bindings as Bindings
import Stage2.Resolve.Context
  ( Context (..),
    (!=),
    (!=~),
  )
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage2.Temporary.PatternInfix as Infix (fix, resolve)
import Stage2.Tree.PatternField (Field (..))
import qualified Stage2.Tree.PatternField as Field (neverFails, resolve)
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)
import qualified Prelude

data Pattern scope
  = Wildcard
      { names :: !(Map Variable Position)
      }
  | Constructor
      { constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern scope)),
        single :: !Prelude.Bool
      }
  | Record
      { constructorPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope)),
        single :: !Prelude.Bool
      }
  | Integer
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        integer :: !Integer
      }
  | Float
      { startPosition :: !Position,
        names :: !(Map Variable Position),
        irrefutable :: !Prelude.Bool,
        float :: !Rational
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
        items :: !(Strict.Vector1 (Pattern scope))
      }
  deriving (Show)

instance Shift Pattern where
  shift = shiftDefault

instance Shift.Functor Pattern where
  map category = \case
    Wildcard {names} -> Wildcard {names}
    Constructor {names, irrefutable, constructorPosition, constructor, patterns, single} ->
      Constructor
        { names,
          irrefutable,
          constructorPosition,
          constructor = Shift.map category constructor,
          patterns = fmap (Shift.map category) patterns,
          single
        }
    Record {names, irrefutable, constructorPosition, constructor, fields, single} ->
      Record
        { names,
          irrefutable,
          constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields,
          single
        }
    Integer {names, irrefutable, startPosition, integer} -> Integer {names, irrefutable, startPosition, integer}
    Float {names, irrefutable, startPosition, float} -> Float {names, irrefutable, startPosition, float}
    Character {names, irrefutable, startPosition, character} -> Character {names, irrefutable, startPosition, character}
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

lazy :: Pattern scope -> Pattern scope
lazy = \case
  Wildcard {names} -> Wildcard {names}
  Constructor {names, constructorPosition, constructor, patterns, single} ->
    Constructor {names, irrefutable, constructorPosition, constructor, patterns, single}
  Record {names, constructorPosition, constructor, fields, single} ->
    Record {names, irrefutable, constructorPosition, constructor, fields, single}
  Integer {names, startPosition, integer} -> Integer {names, irrefutable, startPosition, integer}
  Float {names, startPosition, float} -> Float {names, irrefutable, startPosition, float}
  Character {names, startPosition, character} -> Character {names, irrefutable, startPosition, character}
  String {names, startPosition, string} -> String {names, irrefutable, startPosition, string}
  List {names, startPosition, items} -> List {names, irrefutable, startPosition, items}
  where
    irrefutable = Prelude.True

augment :: Pattern scope' -> Context scope -> Context (Scope.Pattern ':+ scope)
augment patternx context
  | context@Context {locals} <- shift context =
      context
        { locals = bindings patternx </> locals
        }

bindings :: (Monoid stability) => Pattern scope' -> Bindings.Bindings stability (Scope.Pattern ':+ scope)
bindings patternx =
  Bindings.Bindings
    { terms = Map.map termIndex (selections patternx),
      constructors = Map.empty,
      types = Map.empty,
      stability = mempty
    }
  where
    termIndex (position :@ x) =
      Term.Binding
        { position,
          index = Term2.index $ Term.Pattern x,
          fixity = Fixity {associativity = Left, precedence = 9},
          selector = Term.Normal
        }

selections :: Pattern scope -> Map Variable (Marked Term.Bound Position)
selections patternx = Map.map single (selections patternx)
  where
    single = \case
      [] -> error "missing bound"
      [(position, patternx)] -> position :@ patternx
      patterns -> duplicateVariableEntries (map fst patterns)

    selections :: Pattern scope -> Map Variable [(Position, Term.Bound)]
    selections patternx = Map.unionWith (++) outer inner
      where
        outer = Map.map (\position -> [(position, Term.At)]) (names patternx)
        select :: [Pattern scope] -> Map Variable [(Position, Term.Bound)]
        select patterns = foldr (Map.unionWith (++)) Map.empty $ zipWith pick patterns [0 ..]
          where
            pick :: Pattern scope -> Int -> Map Variable [(Position, Term.Bound)]
            pick patternx i = (Map.map . map) (\(position, x) -> (position, Term.Select i x)) (selections patternx)
        inner = case patternx of
          Wildcard {} -> select []
          Constructor {patterns} -> select (toList patterns)
          Record {fields} -> select [patternx | Field _ patternx <- toList fields]
          Integer {} -> select []
          Float {} -> select []
          Character {} -> select []
          String {} -> select []
          List {items} -> select (toList items)

neverFails :: Pattern scope -> Prelude.Bool
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

resolve :: Context scope -> Stage1.Pattern Position -> Pattern scope
resolve context = \case
  Stage1.Variable {variable = position :@ name} -> variable position name
  Stage1.At {variable = position :@ variable, patternx} ->
    let pre = resolve context patternx
     in pre {names = Map.insert variable position (names pre)}
  Stage1.Wildcard {} -> Wildcard {names = Map.empty}
  Stage1.Infix {startPosition, left, operator, operatorPosition, right} ->
    Infix.fix $
      Infix.resolve
        context
        Stage1.Infix.Infix
          { startPosition,
            left,
            operator,
            operatorPosition,
            right
          }
  Stage1.InfixCons {startPosition, left, operatorPosition, right} ->
    Infix.fix $
      Infix.resolve
        context
        Stage1.Infix.InfixCons
          { startPosition,
            left,
            operatorPosition,
            right
          }
  Stage1.Irrefutable {patternx} -> lazy (resolve context patternx)
  Stage1.Constructor {startPosition, constructor, patterns} ->
    case context !=~ startPosition :@ constructor of
      Constructor.Binding {index = constructor, single} ->
        Constructor
          { names = Map.empty,
            irrefutable = Prelude.False,
            constructorPosition = startPosition,
            constructor,
            patterns = fmap (resolve context) patterns,
            single
          }
  Stage1.Record {startPosition, constructor, fields} ->
    case context != startPosition :@ constructor of
      binding@Constructor.Binding
        { index = constructor,
          single
        } ->
          Record
            { names = Map.empty,
              irrefutable = Prelude.False,
              constructorPosition = startPosition,
              constructor,
              fields = fmap (Field.resolve context binding) fields,
              single
            }
  Stage1.Integer {startPosition, integer} ->
    Integer
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        integer
      }
  Stage1.Float {startPosition, float} ->
    Float
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        float
      }
  Stage1.Character {startPosition, character} ->
    Character
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        character
      }
  Stage1.String {startPosition, string} ->
    String
      { names = Map.empty,
        irrefutable = Prelude.False,
        startPosition,
        string
      }
  Stage1.Unit {startPosition} ->
    Constructor
      { names = Map.empty,
        irrefutable = Prelude.False,
        constructorPosition = startPosition,
        constructor = Constructor.tuple 0,
        patterns = Strict.Vector.empty,
        single = Prelude.True
      }
  Stage1.Tuple {startPosition, elements} ->
    Constructor
      { names = Map.empty,
        irrefutable = Prelude.False,
        constructorPosition = startPosition,
        constructor = Constructor.tuple (length elements),
        patterns = resolve context <$> Strict.Vector2.toVector elements,
        single = Prelude.True
      }
  Stage1.List {startPosition, items}
    | null items ->
        Constructor
          { names = Map.empty,
            irrefutable = Prelude.False,
            constructorPosition = startPosition,
            constructor = Constructor.nil,
            patterns = Strict.Vector.empty,
            single = Prelude.False
          }
    | otherwise ->
        List
          { names = Map.empty,
            irrefutable = Prelude.False,
            startPosition,
            items = fmap (resolve context) (Strict.Vector1.fromVector items)
          }
  Stage1.Cons {startPosition, head, tail} ->
    Constructor
      { names = Map.empty,
        irrefutable = Prelude.False,
        constructorPosition = startPosition,
        constructor = Constructor.cons,
        patterns = Strict.Vector.fromList [resolve context head, resolve context tail],
        single = Prelude.False
      }

variable :: Position -> Variable -> Pattern scope
variable position name = Wildcard {names = Map.singleton name position}
