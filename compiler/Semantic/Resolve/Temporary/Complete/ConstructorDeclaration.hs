{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Temporary.Complete.ConstructorDeclaration where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Strict.Maybe as Strict (Maybe, isJust)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( duplicateConstructorEntries,
    duplicateFixityEntries,
    duplicateUnorderedEntries,
    missingConstructorDeclaration,
  )
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Semantic.Resolve.Temporary.Partial.ConstructorDeclaration as Source
import qualified Semantic.Resolve.Temporary.Partial.More.Constructor as More
import Syntax.Extensions (Extensions (..))
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Constructor, Variable)
import Prelude hiding (Either (Left, Right))

data ConstructorDeclaration = Constructor
  { position :: !Position,
    name :: !Constructor,
    typeIndex :: !Int,
    constructorIndex :: !Int,
    fixity :: !Fixity,
    fields :: !(Map Variable Int),
    selections :: !(Strict.Vector (Strict.Maybe Int)),
    unordered :: !Bool,
    fielded :: !Bool,
    single :: !Bool
  }
  deriving (Show)

indexes :: Strict.Vector ConstructorDeclaration -> Map Constructor Int
indexes constructors =
  Map.fromList $ zip [name constructor | constructor <- toList constructors] [0 ..]

merge :: Extensions -> NonEmpty Source.ConstructorDeclaration -> ConstructorDeclaration
merge ~Extensions {unorderedRecords, constructorFields} entries@(entry :| _) =
  Constructor
    { position,
      name,
      typeIndex,
      constructorIndex,
      fixity,
      fields,
      selections,
      unordered,
      fielded,
      single
    }
  where
    position = Source.position entry
    name = Source.name entry
    fixity = case mapMaybe fixity (toList entries) of
      [] -> Fixity {associativity = Left, precedence = 9}
      [(_, fixity)] -> fixity
      fixities -> duplicateFixityEntries (map fst fixities)
      where
        fixity = \case
          Source.Fixity {position, fixity} -> Just (position, fixity)
          _ -> Nothing
    More.Constructor {typeIndex, constructorIndex, selections, fields, single} =
      case mapMaybe index (toList entries) of
        [] -> missingConstructorDeclaration position
        [(_, constructor)] -> constructor
        indexes -> duplicateConstructorEntries (map fst indexes)
      where
        index = \case
          Source.Declaration {position, constructor} ->
            Just (position, constructor)
          _ -> Nothing
    unordered = case mapMaybe unordered (toList entries) of
      [] -> unorderedRecords && any Strict.isJust selections
      [_] -> True
      unordereds -> duplicateUnorderedEntries unordereds
      where
        unordered = \case
          Source.Unordered {position} -> Just position
          _ -> Nothing
    fielded = case mapMaybe fielded (toList entries) of
      [] -> constructorFields
      [_] -> True
      unordereds -> duplicateUnorderedEntries unordereds
      where
        fielded = \case
          Source.Fields {position} -> Just position
          _ -> Nothing

bindings ::
  (Int -> Type0.Index scope) ->
  Strict.Vector ConstructorDeclaration ->
  Map Constructor (Constructor.Binding scope)
bindings index constructors = Map.map constructorIndex (indexes constructors)
  where
    constructorIndex vectorIndex =
      Constructor.Binding
        { position,
          index =
            Constructor.Index
              { typeIndex = Type2.Index $ Type0.normal $ index typeIndex,
                constructorIndex
              },
          fixity,
          fields,
          selections,
          unordered,
          fielded,
          single
        }
      where
        Constructor
          { position,
            fixity,
            fields,
            selections,
            unordered,
            typeIndex,
            constructorIndex,
            fielded,
            single
          } = constructors Strict.Vector.! vectorIndex
