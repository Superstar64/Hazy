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
    (!=*~),
  )
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage2.Temporary.PatternInfix as Infix (fix, resolve)
import Stage2.Tree.PatternField (Field (..))
import qualified Stage2.Tree.PatternField as Field (resolve)
import Prelude hiding (Bool (False, True), Either (Left, Right), head, tail)

data Pattern scope = At
  { names :: !(Map Variable Position),
    match :: !(Match scope)
  }
  deriving (Show)

instance Shift Pattern where
  shift = shiftDefault

instance Shift.Functor Pattern where
  map category At {names, match} =
    At
      { names,
        match = Shift.map category match
      }

data Match scope
  = Wildcard
  | Irrefutable !(Bindings scope)
  | Match !(Bindings scope)
  deriving (Show)

instance Shift Match where
  shift = shiftDefault

instance Shift.Functor Match where
  map category = \case
    Wildcard -> Wildcard
    Irrefutable bindings -> Irrefutable (Shift.map category bindings)
    Match bindings -> Match (Shift.map category bindings)

data Bindings scope
  = Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern scope))
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field scope))
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !Text
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector1 (Pattern scope))
      }
  deriving (Show)

instance Shift Bindings where
  shift = shiftDefault

instance Shift.Functor Bindings where
  map category = \case
    Constructor {constructorPosition, constructor, patterns} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor,
          patterns = fmap (Shift.map category) patterns
        }
    Record {constructorPosition, constructor, fields} ->
      Record
        { constructorPosition,
          constructor = Shift.map category constructor,
          fields = fmap (Shift.map category) fields
        }
    Integer {startPosition, integer} -> Integer {startPosition, integer}
    Float {startPosition, float} -> Float {startPosition, float}
    Character {startPosition, character} -> Character {startPosition, character}
    String {startPosition, string} ->
      String
        { startPosition,
          string
        }
    List {startPosition, items} ->
      List
        { startPosition,
          items = fmap (Shift.map category) items
        }

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
        { Term.position,
          Term.index = Term2.index $ Term.Pattern x,
          Term.fixity = Fixity Left 9,
          Term.selector = Term.Normal
        }

selections :: Pattern scope -> Map Variable (Marked Term.Bound Position)
selections patternx = Map.map single (selections patternx)
  where
    single = \case
      [] -> error "missing bound"
      [(position, patternx)] -> position :@ patternx
      patterns -> duplicateVariableEntries (map fst patterns)

    selections :: Pattern scope -> Map Variable [(Position, Term.Bound)]
    selections (At {names, match}) = Map.unionWith (++) outer inner
      where
        outer = Map.map (\position -> [(position, Term.At)]) names
        inner = case match of
          Wildcard -> Map.empty
          Match patternx -> go patternx
          Irrefutable patternx -> go patternx
        select :: [Pattern scope] -> Map Variable [(Position, Term.Bound)]
        select patterns = foldr (Map.unionWith (++)) Map.empty $ zipWith pick patterns [0 ..]
          where
            pick :: Pattern scope -> Int -> Map Variable [(Position, Term.Bound)]
            pick patternx i = (Map.map . map) (\(position, x) -> (position, Term.Select i x)) (selections patternx)
        go = \case
          Constructor {patterns} -> select (toList patterns)
          Record {fields} -> select [patternx | Field _ patternx <- toList fields]
          Integer {} -> select []
          Float {} -> select []
          Character {} -> select []
          String {} -> select []
          List {items} -> select (toList items)

resolve :: Context scope -> Stage1.Pattern Position -> Pattern scope
resolve context = \case
  Stage1.Variable {Stage1.variable = position :@ name} -> variable position name
  Stage1.At {Stage1.variable = position :@ variable, Stage1.patternx} ->
    At {names = Map.insert variable position names, match}
    where
      At {names, match} = resolve context patternx
  Stage1.Wildcard {} -> At {names = Map.empty, match = Wildcard}
  Stage1.Infix {Stage1.startPosition, Stage1.left, Stage1.operator, Stage1.operatorPosition, Stage1.right} ->
    Infix.fix $
      Infix.resolve
        context
        Stage1.Infix.Infix
          { Stage1.Infix.startPosition,
            Stage1.Infix.left,
            Stage1.Infix.operator,
            Stage1.Infix.operatorPosition,
            Stage1.Infix.right
          }
  Stage1.InfixCons {Stage1.startPosition, Stage1.left, Stage1.operatorPosition, Stage1.right} ->
    Infix.fix $
      Infix.resolve
        context
        Stage1.Infix.InfixCons
          { Stage1.Infix.startPosition,
            Stage1.Infix.left,
            Stage1.Infix.operatorPosition,
            Stage1.Infix.right
          }
  Stage1.Irrefutable {Stage1.patternx} ->
    let At {names, match} = resolve context patternx
     in At
          { names,
            match = case match of
              Match patternx -> Irrefutable patternx
              patternx -> patternx
          }
  pattern1 -> At {names = Map.empty, match}
    where
      match = Match $ case pattern1 of
        Stage1.Constructor {Stage1.startPosition, Stage1.constructor, Stage1.patterns} ->
          case context !=*~ startPosition :@ constructor of
            constructor ->
              Constructor
                { constructorPosition = startPosition,
                  constructor,
                  patterns = fmap (resolve context) patterns
                }
        Stage1.Record {Stage1.startPosition, Stage1.constructor, Stage1.fields} ->
          case context != startPosition :@ constructor of
            binding@Constructor.Binding
              { Constructor.index = constructor
              } ->
                Record
                  { constructorPosition = startPosition,
                    constructor,
                    fields = fmap (Field.resolve context binding) fields
                  }
        Stage1.Integer {Stage1.startPosition, Stage1.integer} ->
          Integer
            { startPosition,
              integer
            }
        Stage1.Float {Stage1.startPosition, Stage1.float} ->
          Float
            { startPosition,
              float
            }
        Stage1.Character {Stage1.startPosition, Stage1.character} -> Character {startPosition, character}
        Stage1.String {Stage1.startPosition, Stage1.string} ->
          String
            { startPosition,
              string
            }
        Stage1.Unit {Stage1.startPosition} ->
          Constructor
            { constructorPosition = startPosition,
              constructor = Constructor.tuple 0,
              patterns = Strict.Vector.empty
            }
        Stage1.Tuple {Stage1.startPosition, Stage1.elements} ->
          Constructor
            { constructorPosition = startPosition,
              constructor = Constructor.tuple (length elements),
              patterns = resolve context <$> Strict.Vector2.toVector elements
            }
        Stage1.List {Stage1.startPosition, Stage1.items}
          | null items ->
              Constructor
                { constructorPosition = startPosition,
                  constructor = Constructor.nil,
                  patterns = Strict.Vector.empty
                }
          | otherwise ->
              List
                { startPosition,
                  items = fmap (resolve context) (Strict.Vector1.fromVector items)
                }
        Stage1.Cons {Stage1.startPosition, Stage1.head, Stage1.tail} ->
          Constructor
            { constructorPosition = startPosition,
              constructor = Constructor.cons,
              patterns = Strict.Vector.fromList [resolve context head, resolve context tail]
            }

variable :: Position -> Variable -> Pattern scope
variable position name = At {names = Map.singleton name position, match = Wildcard}
