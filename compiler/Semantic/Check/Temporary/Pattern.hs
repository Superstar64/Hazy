module Semantic.Check.Temporary.Pattern where

import Control.Monad (when)
import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import Core.Tree.Data (TypesInfo (TypesInfo))
import qualified Core.Tree.Data as Data
import qualified Core.Tree.Forall as Core (mono)
import Core.Tree.Type ((#))
import qualified Core.Tree.Type as Core (char, constructor, list, typex)
import Core.Tree.TypeDeclaration (assumeData)
import Core.Type.Functor (shiftLogical)
import Data.Map (Map)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( mismatchedConstructorArguments,
  )
import Semantic.Check.Binding.Term (TermBinding (TermBinding), Type (..))
import qualified Semantic.Check.Binding.Type as TypeBinding
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import qualified Semantic.Check.Instanciate.Data as DataInstance
import Semantic.Check.Temporary.ConstructorInfo (ConstructorInfo)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type
import Semantic.Index.Term (Bound)
import qualified Semantic.Index.Term as Bound (Bound (..))
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Pattern (Failable (Unused))
import qualified Semantic.Tree.Pattern as Semantic
import qualified Semantic.Tree.Pattern as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.StringLiteral (StringLiteral)
import Syntax.Variable (Variable)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern s scope
  = Wildcard
      { names :: !(Map Variable Position),
        typex :: !(Unify.Type s scope)
      }
  | Match
      { names :: !(Map Variable Position),
        typex :: !(Unify.Type s scope),
        irrefutable :: !Prelude.Bool,
        match :: !(Bindings s scope)
      }

data Bindings s scope
  = Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern s scope)),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Record
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field s scope)),
        constructorInfo :: !(ConstructorInfo s scope)
      }
  | Integer
      { startPosition :: !Position,
        integer :: !Integer,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Float
      { startPosition :: !Position,
        float :: !Rational,
        evidence :: !(Unify.Evidence s scope),
        equal :: !(Unify.Evidence s scope)
      }
  | Character
      { startPosition :: !Position,
        character :: !Char
      }
  | String
      { startPosition :: !Position,
        string :: !StringLiteral
      }
  | List
      { startPosition :: !Position,
        items :: !(Strict.Vector1 (Pattern s scope))
      }

data Field s scope = Field
  { index :: !Int,
    field :: !(Pattern s scope)
  }

(!) :: Pattern s scope -> Bound -> Unify.Type s scope
patternx ! Bound.At = typex patternx
patternx ! Bound.Select index bound = case patternx of
  Wildcard {} -> error "bad index"
  Match {match} -> case match of
    Constructor {patterns} -> patterns Strict.Vector.! index ! bound
    Record {fields} -> fields Strict.Vector.! index !! bound
      where
        Field {field} !! bound = field ! bound
    List {items} -> items Strict.Vector.! index ! bound
    Integer {} -> error "bad index"
    Float {} -> error "bad index"
    Character {} -> error "bad index"
    String {} -> error "bad index"

augment :: Pattern s scope -> Context s scope -> Context s (Scope.Pattern ':+ scope)
augment patternx Context {termEnvironment, localEnvironment, typeEnvironment} =
  Context
    { termEnvironment = Term.Pattern (augmentPattern patternx) termEnvironment,
      localEnvironment = Local.Pattern localEnvironment,
      typeEnvironment = Type.Pattern typeEnvironment
    }

augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentPattern patternx = Term.Bound {at, select}
  where
    at = TermBinding $ pure $ Wobbly (shiftLogical (Core.mono $ typex patternx))
    select = case patternx of
      Wildcard {} -> Strict.Vector.empty
      Match {match} -> case match of
        List {items} -> Strict.Vector.map augmentPattern items
        Constructor {patterns} -> fmap augmentPattern patterns
        Record {fields} -> fmap augmentField fields
          where
            augmentField Field {field} = augmentPattern field
        Integer {} -> Strict.Vector.empty
        Float {} -> Strict.Vector.empty
        Character {} -> Strict.Vector.empty
        String {} -> Strict.Vector.empty

check ::
  Context s scope ->
  Unify.Type s scope ->
  Semantic.Pattern Position Resolve scope ->
  ST s (Pattern s scope)
check context@Context {typeEnvironment} typex = \case
  Semantic.Wildcard {names} -> pure Wildcard {typex, names}
  Semantic.Match {names, irrefutable, match} -> do
    match <- case match of
      Semantic.Constructor
        { constructorPosition,
          constructor,
          patterns
        } ->
          do
            let Constructor.Index typeIndex constructorIndex = constructor
            datax <- do
              let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
              Builtin.index pure get typeIndex
            Data.Definition {types, constructors} <-
              DataInstance.instanciate context constructorPosition datax
            let root = Core.constructor typeIndex
                base = foldl (#) root $ case types of
                  TypesInfo types -> types
                instancex = constructors Strict.Vector.! constructorIndex
                entries = ConstructorInstance.types instancex
                constructorInfo = ConstructorInstance.info instancex
            ConstructorInstance.mark context instancex
            Unify.unify context constructorPosition typex base
            when (length entries /= length patterns) $ mismatchedConstructorArguments constructorPosition
            patterns <- sequence $ Strict.Vector.zipWith (check context) entries patterns
            pure $
              Constructor
                { constructorPosition,
                  constructor,
                  patterns,
                  constructorInfo
                }
      Semantic.Record
        { constructorPosition,
          constructor,
          fields
        } -> do
          let Constructor.Index typeIndex constructorIndex = constructor
          datax <- do
            let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
            Builtin.index pure get typeIndex
          Data.Definition {types, constructors} <-
            DataInstance.instanciate context constructorPosition datax
          let root = Core.constructor typeIndex
              base = foldl (#) root $ case types of
                TypesInfo types -> types
              instancex = constructors Strict.Vector.! constructorIndex
              entries = ConstructorInstance.types instancex
              constructorInfo = ConstructorInstance.info instancex
              lookup index = entries Strict.Vector.! index
              checkPattern Semantic.Field {index, field} = do
                field <- check context (lookup index) field
                pure
                  Field
                    { index,
                      field
                    }
          ConstructorInstance.mark context instancex
          Unify.unify context constructorPosition typex base
          fields <- traverse checkPattern fields
          pure $
            Record
              { constructorPosition,
                constructor,
                fields,
                constructorInfo
              }
      Semantic.List {startPosition, items} -> do
        element <- Unify.fresh Core.typex
        items <- traverse (check context element) items
        Unify.unify context startPosition typex (Core.list element)
        pure $ List {startPosition, items}
      Semantic.Character {startPosition, character} -> do
        Unify.unify context startPosition typex Core.char
        pure $ Character {startPosition, character}
      Semantic.String {startPosition, string} -> do
        Unify.unify context startPosition typex (Core.list Core.char)
        pure $ String {startPosition, string}
      Semantic.Integer {startPosition, integer} -> do
        evidence <- Unify.constrain context startPosition Type2.Num typex
        equal <- Unify.constrain context startPosition Type2.Eq typex
        pure Integer {startPosition, integer, evidence, equal}
      Semantic.Float {startPosition, float} -> do
        evidence <- Unify.constrain context startPosition Type2.Fractional typex
        equal <- Unify.constrain context startPosition Type2.Eq typex
        pure Float {startPosition, float, evidence, equal}
    pure Match {typex, names, irrefutable, match}

solve :: Pattern s scope -> Unify.Solve s (Solved.Pattern Position Check scope)
solve = \case
  Wildcard {names} -> pure Solved.Wildcard {names}
  Match {names, irrefutable, match} -> do
    match <- case match of
      Constructor
        { constructorPosition,
          constructor,
          patterns,
          constructorInfo
        } -> do
          patterns <- traverse solve patterns
          constructorInfo <- ConstructorInfo.solve constructorInfo
          pure
            Solved.Constructor
              { constructorPosition,
                constructor,
                patterns,
                constructorInfo = Solved constructorInfo,
                failable = Unused
              }
      Record
        { constructorPosition,
          constructor,
          fields,
          constructorInfo
        } -> do
          let solvePattern Field {index, field} = do
                field <- solve field
                pure $ Solved.Field {index, field}
          fields <- traverse solvePattern fields
          constructorInfo <- ConstructorInfo.solve constructorInfo
          pure
            Solved.Record
              { constructorPosition,
                constructor,
                fields,
                constructorInfo = Solved constructorInfo,
                failable = Unused
              }
      List {startPosition, items} -> do
        items <- traverse solve items
        pure Solved.List {startPosition, items}
      Integer {startPosition, integer, evidence, equal} -> do
        evidence <- Unify.solveEvidence startPosition evidence
        equal <- Unify.solveEvidence startPosition equal
        pure
          Solved.Integer
            { startPosition,
              integer,
              evidence = Solved evidence,
              equal = Solved equal
            }
      Float {startPosition, float, evidence, equal} -> do
        evidence <- Unify.solveEvidence startPosition evidence
        equal <- Unify.solveEvidence startPosition equal
        pure
          Solved.Float
            { float,
              startPosition,
              evidence = Solved evidence,
              equal = Solved equal
            }
      Character {startPosition, character} ->
        pure $
          Solved.Character
            { startPosition,
              character
            }
      String {startPosition, string} ->
        pure $
          Solved.String
            { startPosition,
              string
            }
    pure Solved.Match {names, irrefutable, match}
