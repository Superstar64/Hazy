{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Partial.ConstructorDeclaration where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1 (Declaration (..))
import Stage1.Tree.Fixity (Fixity)
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (Constructor (..), ConstructorIdentifier, Name (..))
import qualified Stage2.Temporary.Complete.Constructor as Complete (Constructor (Constructor))
import qualified Stage2.Temporary.Complete.Constructor as Complete.Constructor
import Stage2.Temporary.Complete.Field (Field (Field))
import qualified Stage2.Temporary.Complete.Field as Field
import qualified Stage2.Temporary.Complete.GADTConstructor as Complete (GADTConstructor (GADTConstructor))
import qualified Stage2.Temporary.Complete.GADTConstructor as Complete.GADTConstructor
import qualified Stage2.Temporary.Complete.TypeDeclaration as Complete
  ( Constructors (..),
    TypeDeclaration
      ( TypeDeclaration,
        constructors
      ),
  )
import qualified Stage2.Temporary.Partial.More.Constructor as More

data ConstructorDeclaration
  = Fixity
      { position :: !Position,
        name :: !Constructor,
        fixity :: !Fixity
      }
  | Declaration
      { position :: !Position,
        name :: !Constructor,
        constructor :: !More.Constructor
      }
  | Unordered
      { position :: !Position,
        name :: !Constructor
      }
  | Fields
      { position :: !Position,
        name :: !Constructor
      }

resolve ::
  (ConstructorIdentifier -> Maybe (Int, Complete.TypeDeclaration scope)) ->
  Stage1.Declaration Position ->
  [(Constructor, ConstructorDeclaration)]
resolve lookup entry = case entry of
  Stage1.Infix {fixity, termNames'} -> do
    position :@ Constructor name <- toList termNames'
    pure (name, Fixity {position, name, fixity})
  Stage1.Unordered {constructorNames} -> do
    position :@ name <- toList constructorNames
    pure (name, Unordered {position, name})
  Stage1.Fields {constructorNames} -> do
    position :@ name <- toList constructorNames
    pure (name, Fields {position, name})
  Stage1.Data {typeName = name}
    | Just (typeIndex, declaration) <- lookup name -> case declaration of
        Complete.TypeDeclaration {constructors = Complete.ADT constructors} ->
          zipWith entry [0 ..] $ toList constructors
          where
            entry
              constructorIndex
              Complete.Constructor {name, position, fields, selections} =
                (name, Declaration {position, name, constructor})
                where
                  map = [(name, i) | (i, Field {name}) <- zip [0 ..] $ toList fields]
                  constructor =
                    More.Constructor
                      { typeIndex,
                        constructorIndex,
                        selections,
                        fields = Map.fromList map
                      }
        Complete.TypeDeclaration {constructors = Complete.GADT gadtConstructors} ->
          zipWith entry [0 ..] $ toList gadtConstructors
          where
            entry
              constructorIndex
              Complete.GADTConstructor
                { name,
                  position
                } =
                (name, Declaration {position, name, constructor})
                where
                  selections = Strict.Vector.empty
                  fields = Map.empty
                  constructor =
                    More.Constructor
                      { typeIndex,
                        constructorIndex,
                        selections,
                        fields
                      }
        _ -> error "constructor from non data"
  _ -> []
