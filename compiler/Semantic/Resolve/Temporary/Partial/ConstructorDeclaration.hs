{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Temporary.Partial.ConstructorDeclaration where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Resolve.Temporary.Complete.Constructor as Complete (Constructor (Constructor))
import qualified Semantic.Resolve.Temporary.Complete.Constructor as Complete.Constructor
import Semantic.Resolve.Temporary.Complete.Field (Field (Field))
import qualified Semantic.Resolve.Temporary.Complete.Field as Field
import qualified Semantic.Resolve.Temporary.Complete.GADTConstructor as Complete (GADTConstructor (GADTConstructor))
import qualified Semantic.Resolve.Temporary.Complete.GADTConstructor as Complete.GADTConstructor
import qualified Semantic.Resolve.Temporary.Complete.TypeDeclaration as Complete
  ( Constructors (..),
    TypeDeclaration
      ( TypeDeclaration,
        constructors
      ),
  )
import qualified Semantic.Resolve.Temporary.Partial.More.Constructor as More
import Syntax.Position (Position)
import qualified Syntax.Tree.Declaration as Syntax (Declaration (..))
import Syntax.Tree.Fixity (Fixity)
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (Constructor (..), ConstructorIdentifier, Name (..))

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
  Syntax.Declaration Position ->
  [(Constructor, ConstructorDeclaration)]
resolve lookup entry = case entry of
  Syntax.Infix {fixity, termNames'} -> do
    position :@ Constructor name <- toList termNames'
    pure (name, Fixity {position, name, fixity})
  Syntax.Unordered {constructorNames} -> do
    position :@ name <- toList constructorNames
    pure (name, Unordered {position, name})
  Syntax.Fields {constructorNames} -> do
    position :@ name <- toList constructorNames
    pure (name, Fields {position, name})
  Syntax.Data {typeName = name}
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
                        fields = Map.fromList map,
                        single = length constructors == 1
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
                        fields,
                        single = length gadtConstructors == 1
                      }
        _ -> error "constructor from non data"
  _ -> []
