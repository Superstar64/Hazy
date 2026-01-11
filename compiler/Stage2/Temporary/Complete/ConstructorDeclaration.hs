{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Complete.ConstructorDeclaration where

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
import Stage1.Extensions (Extensions (..))
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Constructor, Variable)
import qualified Stage2.Index.Constructor as Constructor (Index (..))
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import qualified Stage2.Temporary.Partial.ConstructorDeclaration as Source
import qualified Stage2.Temporary.Partial.More.Constructor as More
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
    fielded :: !Bool
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
      fielded
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
    More.Constructor {typeIndex, constructorIndex, selections, fields} =
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
  (Int -> Type.Index scope) ->
  Strict.Vector ConstructorDeclaration ->
  Map Constructor (Constructor.Binding scope)
bindings index constructors = Map.map constructorIndex (indexes constructors)
  where
    constructorIndex vectorIndex =
      Constructor.Binding
        { position,
          index = Constructor.Index (Type2.Index $ index typeIndex) constructorIndex,
          fixity,
          fields,
          selections,
          unordered,
          fielded
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
            fielded
          } = constructors Strict.Vector.! vectorIndex
