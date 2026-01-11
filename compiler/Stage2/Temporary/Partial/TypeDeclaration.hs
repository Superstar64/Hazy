{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Partial.TypeDeclaration where

import Data.Foldable (toList)
import Data.List (find)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error
  ( duplicateConstructorEntries,
    mismatchSelectorTypes,
  )
import Order (orderList', orderNonEmpty', orderWith')
import Stage1.Position (Position)
import qualified Stage1.Tree.ClassDeclarations as Stage1 (ClassDeclarations (..))
import qualified Stage1.Tree.Data as Stage1 (Data (..))
import qualified Stage1.Tree.Declaration as Stage1 (Declaration (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (ConstructorIdentifier)
import Stage2.Resolve.Context (Context (..), augmentLocalTypes)
import Stage2.Temporary.Complete.Constructor (Constructor (Constructor))
import qualified Stage2.Temporary.Complete.Constructor as Constructor
import qualified Stage2.Temporary.Complete.Field as Complete (Field (Field))
import qualified Stage2.Temporary.Complete.Field as Complete.Field
import qualified Stage2.Temporary.Complete.GADTConstructor as GADTConstructor
import qualified Stage2.Temporary.Complete.Method as Method (merge)
import qualified Stage2.Temporary.Complete.Selector as Complete (Selector (Selector))
import qualified Stage2.Temporary.Complete.Selector as Complete.Selector
import qualified Stage2.Temporary.Partial.Method as Method (resolve)
import qualified Stage2.Temporary.Partial.More.ADT as More (ADT (ADT))
import qualified Stage2.Temporary.Partial.More.ADT as More.ADT
import qualified Stage2.Temporary.Partial.More.Class as More (Class (Class))
import qualified Stage2.Temporary.Partial.More.Class as More.Class
import qualified Stage2.Temporary.Partial.More.GADT as More (GADT (GADT))
import qualified Stage2.Temporary.Partial.More.GADT as More.GADT
import qualified Stage2.Temporary.Partial.More.Synonym as More (Synonym (Synonym))
import qualified Stage2.Temporary.Partial.More.Synonym as More.Synonym
import qualified Stage2.Tree.Constraint as Constraint
import qualified Stage2.Tree.Entry as Entry
import qualified Stage2.Tree.Field as Field
import qualified Stage2.Tree.Selector as Selector
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.Type as Type (resolve)
import qualified Stage2.Tree.TypePattern as TypePattern

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
        annotation :: Type Position scope
      }

resolve ::
  Context scope ->
  Stage1.Declaration Position ->
  [(ConstructorIdentifier, TypeDeclaration scope)]
resolve context entry = case entry of
  Stage1.Synonym
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
  Stage1.Data
    { brand,
      startPosition,
      typeName = name,
      parameters,
      dataDefinition
    }
      | parameters <- TypePattern.resolve <$> parameters ->
          let item = case dataDefinition of
                Stage1.ADT {constructors = constructorsx} ->
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
                              strict = Entry.strict $ Field.entry field
                              indexes = fmap fst <$> fields
                              stricts = fmap (Entry.strict . Field.entry . snd) <$> fields
                              typex = Entry.anonymize $ Field.entry field
                              types = fmap (Entry.anonymize . Field.entry . snd) <$> fields
                              sane
                                | all (== typex) [typex | Strict.Just typex <- toList types] = ()
                                | otherwise = mismatchSelectorTypes position
                              uniform = all (== Strict.Just index) indexes && all (== Strict.Just strict) stricts
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
                                                Selector.Uniform {strict}
                                              else
                                                Selector.Disjoint {indexes}
                                        }
                                  }
                          pure (name, item)
                Stage1.GADT {gadtConstructors = constructors} ->
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
  Stage1.Class
    { startPosition,
      typeName = name,
      parameter,
      superClasses,
      classDefinition
    }
      | parameter <- TypePattern.resolve parameter ->
          let Stage1.ClassDeclarations {declarations} = classDefinition
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
  Stage1.KindAnnotation {typeNames, kindAnnotation} -> do
    position :@ name <- toList typeNames
    let annotation = Type.resolve context kindAnnotation
    pure (name, Annotation {position, name, annotation})
  _ -> []
