{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Temporary.Partial.TypeDeclaration where

import Data.Foldable (toList)
import Data.List (find)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( duplicateConstructorEntries,
    mismatchSelectorTypes,
  )
import Order (orderList', orderNonEmpty', orderWith')
import Semantic.Resolve.Context (Context (..), augmentLocalTypes)
import qualified Semantic.Resolve.Go.Constraint as Constraint
import qualified Semantic.Resolve.Go.Type as Type (resolve)
import qualified Semantic.Resolve.Go.TypePattern as TypePattern
import Semantic.Resolve.Temporary.Complete.Constructor (Constructor (Constructor))
import qualified Semantic.Resolve.Temporary.Complete.Constructor as Constructor
import qualified Semantic.Resolve.Temporary.Complete.Field as Complete (Field (Field))
import qualified Semantic.Resolve.Temporary.Complete.Field as Complete.Field
import qualified Semantic.Resolve.Temporary.Complete.GADTConstructor as GADTConstructor
import qualified Semantic.Resolve.Temporary.Complete.Method as Method (merge)
import qualified Semantic.Resolve.Temporary.Complete.Selector as Complete (Selector (Selector))
import qualified Semantic.Resolve.Temporary.Complete.Selector as Complete.Selector
import qualified Semantic.Resolve.Temporary.Partial.Method as Method (resolve)
import qualified Semantic.Resolve.Temporary.Partial.More.ADT as More (ADT (ADT))
import qualified Semantic.Resolve.Temporary.Partial.More.ADT as More.ADT
import qualified Semantic.Resolve.Temporary.Partial.More.Class as More (Class (Class))
import qualified Semantic.Resolve.Temporary.Partial.More.Class as More.Class
import qualified Semantic.Resolve.Temporary.Partial.More.GADT as More (GADT (GADT))
import qualified Semantic.Resolve.Temporary.Partial.More.GADT as More.GADT
import qualified Semantic.Resolve.Temporary.Partial.More.Synonym as More (Synonym (Synonym))
import qualified Semantic.Resolve.Temporary.Partial.More.Synonym as More.Synonym
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Entry as Entry
import qualified Semantic.Tree.Field as Field
import qualified Semantic.Tree.Selector as Selector
import qualified Semantic.Tree.StrictnessAnnotation as StrictnessAnnotation
import Semantic.Tree.Type (Type)
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand (Boxed))
import qualified Syntax.Tree.ClassDeclarations as Syntax (ClassDeclarations (..))
import qualified Syntax.Tree.Data as Syntax (Data (..))
import qualified Syntax.Tree.Declaration as Syntax (Declaration (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (ConstructorIdentifier)

data TypeDeclaration scope
  = ADT
      { position :: !Position,
        name :: !ConstructorIdentifier,
        adt :: !(More.ADT scope)
      }
  | GADT
      { position :: !Position,
        name :: !ConstructorIdentifier,
        gadt :: !(More.GADT scope)
      }
  | Class
      { position :: !Position,
        name :: !ConstructorIdentifier,
        classx :: !(More.Class scope)
      }
  | Synonym
      { position :: !Position,
        name :: !ConstructorIdentifier,
        synonym :: !(More.Synonym scope)
      }
  | Annotation
      { position :: !Position,
        name :: !ConstructorIdentifier,
        annotation :: Type Position Resolve scope
      }

resolve ::
  Context scope ->
  Syntax.Declaration Position ->
  [(ConstructorIdentifier, TypeDeclaration scope)]
resolve context entry = case entry of
  Syntax.Synonym
    { startPosition,
      typeName = name,
      parameters,
      typeDefinition
    }
      | parameters <- TypePattern.resolve <$> parameters ->
          let item =
                Synonym
                  { position = startPosition,
                    name = name,
                    synonym =
                      More.Synonym
                        { parameters,
                          synonym
                        }
                  }
              synonym = Type.resolve context' typeDefinition
                where
                  context' = augmentLocalTypes parameters context
           in [(name, item)]
  Syntax.Data
    { brand,
      startPosition,
      typeName = name,
      parameters,
      dataDefinition
    }
      | parameters <- TypePattern.resolve <$> parameters ->
          let item = case dataDefinition of
                Syntax.ADT {constructors = constructorsx} ->
                  ADT
                    { position = startPosition,
                      name,
                      adt =
                        More.ADT
                          { brand,
                            parameters,
                            constructors,
                            selectors
                          }
                    }
                  where
                    context' = augmentLocalTypes parameters context
                    constructors = orderList' duplicate $ map entries $ toList constructorsx
                      where
                        duplicate [constructor] = constructor
                        duplicate constructors =
                          duplicateConstructorEntries
                            [position | Constructor {position} <- constructors]
                        entries constructor = (name, entry)
                          where
                            entry@Constructor {name} =
                              Constructor.resolve context' resolve constructor
                              where
                                resolve = fmap Complete.Selector.name selectors
                    selectors = orderWith' const $ foldMap entries (zip [0 ..] $ toList constructors)
                      where
                        entries (first, Constructor {fields}) = do
                          (index, Complete.Field {name, position, field}) <-
                            zip [0 ..] $ toList fields
                          let fields = Strict.Vector.fromList $ do
                                Constructor {fields} <- toList constructors
                                let check (_, Complete.Field {name = name'}) =
                                      name == name'
                                    found = find check $ zip [0 ..] $ toList fields
                                pure $ case found of
                                  Nothing -> Strict.Nothing
                                  Just (index, Complete.Field {field}) ->
                                    Strict.Just (index, field)
                              strictness = StrictnessAnnotation.anonymize . Entry.strict . Field.entry
                              strict = strictness field
                              indexes = fmap fst <$> fields
                              stricts = fmap (strictness . snd) <$> fields
                              typex = Entry.anonymize $ Field.entry field
                              types = fmap (Entry.anonymize . Field.entry . snd) <$> fields
                              sane
                                | all (== typex) [typex | Strict.Just typex <- toList types] = ()
                                | otherwise = mismatchSelectorTypes position
                              uniform
                                | Boxed <- brand =
                                    all (== Strict.Just index) indexes
                                      && all (== Strict.Just strict) stricts
                                | otherwise = False
                          let item =
                                Complete.Selector
                                  { position,
                                    name,
                                    real =
                                      Selector.Selector
                                        { name,
                                          position,
                                          first,
                                          index,
                                          uniform =
                                            if seq sane uniform
                                              then
                                                Selector.Uniform {}
                                              else
                                                Selector.Disjoint {indexes}
                                        }
                                  }
                          pure (name, item)
                Syntax.GADT {gadtConstructors = constructors} ->
                  GADT
                    { position = startPosition,
                      name = name,
                      gadt =
                        More.GADT
                          { brand,
                            parameters,
                            gadtConstructors
                          }
                    }
                  where
                    gadtConstructors = orderList' duplicate $ (=<<) entries $ toList constructors
                      where
                        duplicate [constructor] = constructor
                        duplicate constructors =
                          duplicateConstructorEntries
                            [GADTConstructor.position constructor | constructor <- constructors]
                        entries constructor = [(GADTConstructor.name entry, entry) | entry <- entries]
                          where
                            entries = GADTConstructor.resolve context constructor
           in [(name, item)]
  Syntax.Class
    { startPosition,
      typeName = name,
      parameter,
      superClasses,
      classDefinition
    }
      | parameter <- TypePattern.resolve parameter ->
          let Syntax.ClassDeclarations {declarations} = classDefinition
              item =
                Class
                  { position = startPosition,
                    name,
                    classx =
                      More.Class
                        { parameter,
                          constraints,
                          methods
                        }
                  }
              context' = augmentLocalTypes (Strict.Vector.singleton parameter) context
              methods = orderNonEmpty' Method.merge $ foldMap (Method.resolve context') declarations
              constraints = Constraint.resolve context' <$> superClasses
           in [(name, item)]
  Syntax.KindAnnotation {typeNames, kindAnnotation} -> do
    position :@ name <- toList typeNames
    let annotation = Type.resolve context kindAnnotation
    pure (name, Annotation {position, name, annotation})
  _ -> []
