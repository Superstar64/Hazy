{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for declarations
module Stage1.Tree.Declaration where

import Data.Foldable (toList)
import qualified Data.Strict.Vector1 as Strict (Vector1, fromNonEmpty)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenFieldPragma,
    betweenParens,
    betweenUnorderedPragma,
    many,
    position,
    sepBy1Comma,
    sepByComma,
    token,
    try,
    (<**>),
  )
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables (..))
import Stage1.Tree.Alias (Alias)
import Stage1.Tree.Brand (Brand)
import qualified Stage1.Tree.Brand as Brand
import Stage1.Tree.ClassDeclaration (ClassDeclaration)
import qualified Stage1.Tree.ClassDeclaration as ClassDeclaration
import {-# SOURCE #-} Stage1.Tree.ClassDeclarations (ClassDeclarations)
import {-# SOURCE #-} qualified Stage1.Tree.ClassDeclarations as ClassDeclarations
import Stage1.Tree.Constraint (Constraint)
import qualified Stage1.Tree.Constraint as Constraint
import Stage1.Tree.Data (Data)
import qualified Stage1.Tree.Data as Data
import Stage1.Tree.Fixity (Fixity)
import Stage1.Tree.Import (Import)
import qualified Stage1.Tree.Import as Import
import Stage1.Tree.ImportSymbols (Symbols)
import {-# SOURCE #-} Stage1.Tree.InstanceDeclarations (InstanceDeclarations)
import {-# SOURCE #-} qualified Stage1.Tree.InstanceDeclarations as InstanceDeclarations
import Stage1.Tree.InstanceHead (InstanceHead)
import qualified Stage1.Tree.InstanceHead as InstanceHead
import Stage1.Tree.LeftHandSide (LeftHandSide)
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Marked as Marked
import Stage1.Tree.Qualification (Qualification)
import Stage1.Tree.RightHandSide (RightHandSide)
import Stage1.Tree.Scheme (Scheme)
import Stage1.Tree.Type (Type)
import qualified Stage1.Tree.Type as Type
import Stage1.Tree.TypePattern (TypePattern)
import qualified Stage1.Tree.TypePattern as TypePattern
import Stage1.Variable
  ( ConstructorIdentifier,
    FullQualifiers,
    Name (..),
    QualifiedConstructorIdentifier,
    Variable,
  )
import qualified Stage1.Variable as Variable

data Declaration position
  = -- |
    --  > type T = A
    Synonym
      { startPosition :: !position,
        typePosition :: !position,
        typeName :: !ConstructorIdentifier,
        parameters :: !(Strict.Vector (TypePattern Position)),
        typeDefinition :: !(Type position)
      }
  | -- |
    --  > data T
    Data
      { startPosition :: !position,
        brand :: !Brand,
        typePosition :: !position,
        typeName :: !ConstructorIdentifier,
        parameters :: !(Strict.Vector (TypePattern Position)),
        dataDefinition :: !(Data position)
      }
  | -- |
    --  > class C
    Class
      { startPosition :: !position,
        superClasses :: !(Strict.Vector (Constraint position)),
        typePosition :: !position,
        typeName :: !ConstructorIdentifier,
        parameter :: !(TypePattern position),
        classDefinition :: !(ClassDeclarations position)
      }
  | -- |
    --  > instance C T
    Instance
      { startPosition :: !position,
        prerequisites :: !(Strict.Vector (Constraint position)),
        classPosition :: !position,
        className :: !QualifiedConstructorIdentifier,
        instanceHead :: !InstanceHead,
        instanceDefinition :: !(InstanceDeclarations position)
      }
  | -- |
    --  > x :: a
    Annotation
      { termNames :: !(Strict.Vector1 (Marked Variable position)),
        annotation :: !(Scheme position)
      }
  | -- |
    --  > type T :: A
    KindAnnotation
      { typeNames :: !(Strict.Vector1 (Marked ConstructorIdentifier position)),
        kindAnnotation :: !(Type position)
      }
  | -- |
    --  > infix `x`
    Infix
      { fixity :: !Fixity,
        termNames' :: !(Strict.Vector1 (Marked Name position))
      }
  | -- |
    --  > x = e
    Definition
      { startPosition :: !position,
        leftHandSide :: !(LeftHandSide position),
        rightHandSide :: !(RightHandSide position)
      }
  | -- |
    --  > import M
    Import
      { qualification :: !Qualification,
        targetPosition :: !position,
        target :: !FullQualifiers,
        alias :: !Alias,
        symbols :: !Symbols
      }
  | -- |
    -- > {-# UNORDEREDRECORDS A #-}
    Unordered
      { constructorNames :: !(Strict.Vector1 (Marked Variable.Constructor position))
      }
  | -- |
    -- > {-# CONSTRUCTORFIELDS A #-}
    Fields
      { constructorNames :: !(Strict.Vector1 (Marked Variable.Constructor position))
      }
  deriving (Show)

instance TermBindingVariables Declaration where
  termBindingVariables = \case
    Class {classDefinition} -> termBindingVariables classDefinition
    Data {dataDefinition} -> termBindingVariables dataDefinition
    Annotation {termNames} -> toList termNames
    Infix {termNames'} -> [position :@ name | position :@ Variable name <- toList termNames']
    Definition {leftHandSide} -> termBindingVariables leftHandSide
    _ -> []

typeBindingVariables :: Declaration Position -> [(ConstructorIdentifier, Position)]
typeBindingVariables = \case
  Class {typePosition, typeName} -> [(typeName, typePosition)]
  Data {typePosition, typeName} -> [(typeName, typePosition)]
  Synonym {typePosition, typeName} -> [(typeName, typePosition)]
  _ -> []

fromClass :: ClassDeclaration position -> Declaration position
fromClass = \case
  ClassDeclaration.Annotation
    { termNames,
      annotation
    } -> Annotation {termNames, annotation}
  ClassDeclaration.Definition
    { startPosition,
      leftHandSide,
      rightHandSide
    } -> Definition {startPosition, leftHandSide, rightHandSide}
  ClassDeclaration.Infix
    { fixity,
      termNames'
    } -> Infix {fixity, termNames'}

fromImport :: Import position -> Declaration position
fromImport
  Import.Import
    { qualification,
      targetPosition,
      target,
      alias,
      symbols
    } =
    Import
      { qualification,
        targetPosition,
        target,
        alias,
        symbols
      }

toImport :: Declaration position -> Maybe (Import position)
toImport
  Import
    { qualification,
      targetPosition,
      target,
      alias,
      symbols
    } =
    Just
      Import.Import
        { qualification,
          targetPosition,
          target,
          alias,
          symbols
        }
toImport _ = Nothing

parse :: Parser (Declaration Position)
parse =
  asum
    [ datax
        <$> position
        <*> Brand.parse
        <*> position
        <*> Variable.parse
        <*> (Strict.Vector.fromList <$> many TypePattern.parse)
        <*> Data.parse,
      kindAnnotation
        <$> try (token "type" *> (Strict.fromNonEmpty <$> sepBy1Comma Marked.parse) <* token "::")
        <*> Type.parse,
      synonym
        <$> position
        <*> (token "type" *> position)
        <*> Variable.parse
        <*> (Strict.Vector.fromList <$> many TypePattern.parse)
        <*> (token "=" *> Type.parse),
      position <**> (token "class" *> parseClass),
      token "instance" *> parseInstance,
      fromClass <$> ClassDeclaration.parse,
      fromImport <$> Import.parse,
      unordered <$> betweenUnorderedPragma (Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral),
      fields <$> betweenFieldPragma (Strict.fromNonEmpty <$> sepBy1Comma Marked.parseLiteral)
    ]
  where
    unordered constructorNames =
      Unordered
        { constructorNames
        }
    fields constructorNames =
      Fields
        { constructorNames
        }
    kindAnnotation typeNames kindAnnotation =
      KindAnnotation
        { typeNames,
          kindAnnotation
        }
    synonym startPosition typePosition typeName parameters typeDefinition =
      Synonym
        { startPosition,
          typePosition,
          typeName,
          parameters,
          typeDefinition
        }
    datax startPosition brand typePosition typeName parameters dataDefinition =
      Data
        { startPosition,
          brand,
          typePosition,
          typeName,
          parameters,
          dataDefinition
        }
    parseClass :: Parser (Position -> Declaration Position)
    parseClass =
      asum
        [ classx
            <$> try (betweenParens (Strict.Vector.fromList <$> sepByComma Constraint.parse) <* token "=>")
            <*> position
            <*> Variable.parse
            <*> TypePattern.parse
            <*> ClassDeclarations.parse,
          classx
            <$> try (singleConstraint <$> Constraint.parse <* token "=>")
            <*> position
            <*> Variable.parse
            <*> TypePattern.parse
            <*> ClassDeclarations.parse,
          classx Strict.Vector.empty
            <$> position
            <*> Variable.parse
            <*> TypePattern.parse
            <*> ClassDeclarations.parse
        ]
      where
        singleConstraint = Strict.Vector.singleton
        classx superClasses typePosition typeName parameter classDefinition startPosition =
          Class
            { startPosition,
              superClasses,
              typePosition,
              typeName,
              parameter,
              classDefinition
            }

    parseInstance :: Parser (Declaration Position)
    parseInstance =
      asum
        [ try (instancex <$> position <*> constraints <* token "=>")
            <*> position
            <*> Variable.parse
            <*> InstanceHead.parse
            <*> InstanceDeclarations.parse,
          try ((instanceSingle <$> position <*> Constraint.parse) <* token "=>")
            <*> position
            <*> Variable.parse
            <*> InstanceHead.parse
            <*> InstanceDeclarations.parse,
          instanceEmpty
            <$> position
            <*> Variable.parse
            <*> InstanceHead.parse
            <*> InstanceDeclarations.parse
        ]
      where
        constraints = betweenParens (Strict.Vector.fromList <$> sepByComma Constraint.parse)
        instancex startPosition prerequisites classPosition className instanceHead instanceDefinition =
          Instance
            { startPosition,
              prerequisites,
              classPosition,
              className,
              instanceHead,
              instanceDefinition
            }
        instanceSingle startPosition prerequisite classPosition className instanceHead instanceDefinition =
          Instance
            { startPosition,
              prerequisites = Strict.Vector.singleton prerequisite,
              classPosition,
              className,
              instanceHead,
              instanceDefinition
            }
        instanceEmpty classPosition@startPosition className instanceHead instanceDefinition =
          Instance
            { startPosition,
              prerequisites = Strict.Vector.empty,
              classPosition,
              className,
              instanceHead,
              instanceDefinition
            }
