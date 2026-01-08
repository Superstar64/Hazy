module Stage3.Temporary.Pattern where

import Control.Monad (when)
import Control.Monad.ST (ST)
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector1 as Strict.Vector1
import Data.Text (Text)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( mismatchedConstructorArguments,
    unsupportedFeatureFloatingPointLiterals,
    unsupportedFeatureIntegerLiteralPatterns,
  )
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..))
import qualified Stage2.Tree.Pattern as Stage2
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Check.Context (Context (..))
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import Stage3.Check.TermBinding (TermBinding (..), Type (..))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Builtin as Builtin
import qualified Stage3.Simple.Data as Simple.Data
import Stage3.Simple.TypeDeclaration (assumeData)
import Stage3.Temporary.PatternField (Field)
import qualified Stage3.Temporary.PatternField as Field
import qualified Stage3.Tree.Pattern as Solved
import qualified Stage3.Unify as Unify
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Pattern s scope
  = At !(Match s scope) !(Unify.Type s scope)

data Match s scope
  = Wildcard
  | Match {match :: !(Bindings s scope), irrefutable :: !Prelude.Bool}

data Bindings s scope
  = Constructor
      { constructor :: !(Constructor.Index scope),
        patterns :: !(Strict.Vector (Pattern s scope))
      }
  | Record
      { constructor :: !(Constructor.Index scope),
        fields :: !(Strict.Vector (Field s scope)),
        fieldCount :: !Int
      }
  | Character
      { character :: !Char
      }
  | String {string :: !Text}
  | List {items :: !(Strict.Vector1 (Pattern s scope))}

augment :: Pattern s scope -> Context s scope -> Context s (Scope.Pattern ':+ scope)
augment patternx Context {termEnvironment, localEnvironment, typeEnvironment} =
  Context
    { termEnvironment = Term.Pattern (augmentPattern patternx) termEnvironment,
      localEnvironment = Local.Pattern localEnvironment,
      typeEnvironment = Type.Pattern typeEnvironment
    }

augmentPattern :: Pattern s scopes -> Term.Bound (TermBinding s) (scope ':+ scopes)
augmentPattern (At match typex) = Term.Bound {at, select}
  where
    at = TermBinding $ pure $ Wobbly (shift typex)
    select = augmentMatch match

augmentMatch :: Match s scopes -> Strict.Vector (Term.Bound (TermBinding s) (scope ':+ scopes))
augmentMatch match = case match of
  Wildcard -> Strict.Vector.empty
  Match {match} -> case match of
    List items -> Strict.Vector.map augmentPattern $ Strict.Vector1.toVector items
    Constructor {patterns} -> fmap augmentPattern patterns
    Record {fields} -> fmap Field.augmentField fields
    Character {} -> Strict.Vector.empty
    String {} -> Strict.Vector.empty

check :: Context s scope -> Unify.Type s scope -> Stage2.Pattern scope -> ST s (Pattern s scope)
check context@Context {typeEnvironment} typex (Stage2.At {match}) =
  flip At typex <$> case match of
    Stage2.Wildcard -> do
      pure Wildcard
    Stage2.Match match -> do
      match <- go match
      pure Match {match, irrefutable = Prelude.False}
    Stage2.Irrefutable match -> do
      match <- go match
      pure Match {match, irrefutable = Prelude.True}
  where
    go match = case match of
      Stage2.Constructor
        { constructorPosition,
          constructor,
          patterns
        } ->
          do
            let Constructor.Index typeIndex constructorIndex = constructor
            datax <- do
              let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
              Builtin.index pure get typeIndex
            DataInstance {types, constructors} <-
              Simple.Data.instanciate datax
            let root = Unify.constructor typeIndex
                base = foldl Unify.call root types
                ConstructorInstance {entries} =
                  constructors Strict.Vector.! constructorIndex
            Unify.unify context constructorPosition typex base
            when (length entries /= length patterns) $ mismatchedConstructorArguments constructorPosition
            patterns <- sequence $ Strict.Vector.zipWith (check context) entries patterns
            pure $ Constructor {constructor, patterns}
      Stage2.Record {constructorPosition, constructor, fields} -> do
        let Constructor.Index typeIndex constructorIndex = constructor
        datax <- do
          let get index = assumeData <$> TypeBinding.content (typeEnvironment Type.! index)
          Builtin.index pure get typeIndex
        DataInstance {types, constructors} <-
          Simple.Data.instanciate datax
        let root = Unify.constructor typeIndex
            base = foldl Unify.call root types
            ConstructorInstance {entries} =
              constructors Strict.Vector.! constructorIndex
            lookup index = entries Strict.Vector.! index
        Unify.unify context constructorPosition typex base
        fields <- traverse (Field.check context lookup) fields
        pure $ Record {constructor, fields, fieldCount = length entries}
      Stage2.List {startPosition, items} -> do
        element <- Unify.fresh Unify.typex
        items <- traverse (check context element) items
        Unify.unify context startPosition typex (Unify.listWith element)
        pure $ List items
      Stage2.Character {startPosition, character} -> do
        Unify.unify context startPosition typex Unify.char
        pure $ Character {character}
      Stage2.String {startPosition, string} -> do
        Unify.unify context startPosition typex (Unify.listWith Unify.char)
        pure $ String string
      Stage2.Integer {startPosition} ->
        unsupportedFeatureIntegerLiteralPatterns startPosition
      Stage2.Float {startPosition} ->
        unsupportedFeatureFloatingPointLiterals startPosition

solve :: Pattern s scope -> ST s (Solved.Pattern scope)
solve (At match _) = do
  match <- solveMatch match
  pure $ Solved.At match

solveMatch :: Match s scope -> ST s (Solved.Match scope)
solveMatch Wildcard = pure Solved.Wildcard
solveMatch Match {match, irrefutable} = do
  match <- case match of
    Constructor {constructor, patterns} -> Solved.Constructor constructor <$> traverse solve patterns
    Record {constructor, fields, fieldCount} -> do
      fields <- traverse Field.solve fields
      pure Solved.Record {constructor, fields, fieldCount}
    List items -> Solved.List <$> traverse solve items
    Character {character} -> pure $ Solved.Character {character}
    String string -> pure $ Solved.String string
  pure Solved.Match {match, irrefutable}
