module Stage2.Temporary.Complete.TypeDeclaration where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (duplicateTypeEntries, missingTypeDeclaration)
import Stage1.Position (Position)
import Stage1.Variable (ConstructorIdentifier)
import qualified Stage1.Variable as Variable
import qualified Stage2.Index.Type as Type (Index (..))
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Resolve.Binding.Type as Type (Binding (..))
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Temporary.Complete.Constructor (Constructor (Constructor))
import qualified Stage2.Temporary.Complete.Constructor as Constructor
import Stage2.Temporary.Complete.GADTConstructor (GADTConstructor (GADTConstructor))
import qualified Stage2.Temporary.Complete.GADTConstructor as GADTConstructor
import Stage2.Temporary.Complete.Method (Method (Method))
import qualified Stage2.Temporary.Complete.Method as Method
import Stage2.Temporary.Complete.Selector (Selector (Selector))
import qualified Stage2.Temporary.Complete.Selector as Selector
import qualified Stage2.Temporary.Partial.More.ADT as More (ADT (ADT))
import qualified Stage2.Temporary.Partial.More.ADT as More.ADT
import qualified Stage2.Temporary.Partial.More.Class as More (Class (Class))
import qualified Stage2.Temporary.Partial.More.Class as More.Class
import qualified Stage2.Temporary.Partial.More.GADT as More (GADT (GADT))
import qualified Stage2.Temporary.Partial.More.GADT as More.GADT
import qualified Stage2.Temporary.Partial.More.Synonym as More (Synonym (Synonym))
import qualified Stage2.Temporary.Partial.More.Synonym as More.Synonym
import qualified Stage2.Temporary.Partial.TypeDeclaration as Partial
import qualified Stage2.Tree.TypeDeclaration as Real
import Verbose (Debug (resolving))

data Constructors scope
  = ADT !(Strict.Vector (Constructor (Local ':+ scope)))
  | GADT !(Strict.Vector (GADTConstructor scope))
  | NoConstructors

data Fields scope
  = Methods !(Strict.Vector (Method (Local ':+ scope)))
  | Selectors !(Strict.Vector (Selector))
  | NoFields

data TypeDeclaration scope = TypeDeclaration
  { position :: !Position,
    name :: !ConstructorIdentifier,
    fields :: !(Fields scope),
    constructors :: !(Constructors scope),
    declaration :: Real.TypeDeclaration scope
  }

shrink :: TypeDeclaration scope -> Real.TypeDeclaration scope
shrink = declaration

merge :: (Debug verbose) => NonEmpty (Partial.TypeDeclaration scope) -> verbose (TypeDeclaration scope)
merge entries@(entry :| _) =
  let typeDeclaration declaration = TypeDeclaration {position, name, fields, constructors, declaration}
   in typeDeclaration <$> declaration
  where
    declaration = case catMaybes [fmap fst adt, fmap fst gadt, fmap fst classx, fmap fst synonym] of
      [] -> missingTypeDeclaration position
      [_]
        | Just
            ( position,
              More.ADT
                { More.ADT.brand,
                  More.ADT.parameters,
                  More.ADT.constructors,
                  More.ADT.selectors
                }
              ) <-
            adt ->
            Verbose.resolving
              (Variable.print' name)
              $ if
                | constructors <- fmap Constructor.shrink constructors,
                  selectors <- fmap Selector.shrink selectors ->
                    Real.ADT
                      { Real.position,
                        Real.name,
                        Real.brand,
                        Real.parameters,
                        Real.constructors,
                        Real.selectors,
                        Real.annotation
                      }
        | Just
            ( _,
              More.GADT
                { More.GADT.brand,
                  More.GADT.parameters,
                  More.GADT.gadtConstructors
                }
              ) <-
            gadt ->
            Verbose.resolving
              (Variable.print' name)
              Real.GADT
                { Real.position,
                  Real.name,
                  Real.parameters,
                  Real.brand,
                  Real.gadtConstructors = fmap GADTConstructor.shrink gadtConstructors,
                  Real.annotation
                }
        | Just
            ( position,
              More.Class
                { More.Class.parameter,
                  More.Class.constraints,
                  More.Class.methods
                }
              ) <-
            classx ->
            Verbose.resolving
              (Variable.print' name)
              Real.Class
                { Real.position,
                  Real.name,
                  Real.parameter,
                  Real.constraints,
                  Real.methods = fmap Method.shrink methods,
                  Real.annotation
                }
        | Just
            ( _,
              More.Synonym
                { More.Synonym.parameters,
                  More.Synonym.synonym
                }
              ) <-
            synonym ->
            Verbose.resolving
              (Variable.print' name)
              Real.Synonym
                { Real.position,
                  Real.name,
                  Real.parameters,
                  Real.synonym,
                  Real.annotation
                }
      entries -> duplicateTypeEntries entries
    position = Partial.position entry
    name = Partial.name entry
    fields
      | Just (_, More.ADT {More.ADT.selectors}) <- adt = Selectors selectors
      | Just (_, More.GADT {}) <- gadt = Selectors Strict.Vector.empty
      | Just (_, More.Class {More.Class.methods}) <- classx = Methods methods
      | otherwise = NoFields
    constructors
      | Just (_, More.ADT {More.ADT.constructors}) <- adt = ADT constructors
      | Just (_, More.GADT {More.GADT.gadtConstructors}) <- gadt = GADT gadtConstructors
      | otherwise = NoConstructors
    adt = case mapMaybe adt (toList entries) of
      [] -> Nothing
      [adt] -> Just adt
      adts -> duplicateTypeEntries (map fst adts)
      where
        adt = \case
          Partial.ADT
            { Partial.position,
              Partial.adt
            } -> Just (position, adt)
          _ -> Nothing

    gadt = case mapMaybe gadt (toList entries) of
      [] -> Nothing
      [gadt] -> Just gadt
      gadts -> duplicateTypeEntries (map fst gadts)
      where
        gadt = \case
          Partial.GADT
            { Partial.position,
              Partial.gadt
            } -> Just (position, gadt)
          _ -> Nothing

    classx = case mapMaybe classx (toList entries) of
      [] -> Nothing
      [classx] -> Just classx
      classes -> duplicateTypeEntries (map fst classes)
      where
        classx = \case
          Partial.Class {Partial.position, Partial.classx} ->
            Just (position, (classx))
          _ -> Nothing

    synonym = case mapMaybe synonym (toList entries) of
      [] -> Nothing
      [synonym] -> Just synonym
      synonyms -> duplicateTypeEntries (map fst synonyms)
      where
        synonym = \case
          Partial.Synonym {Partial.position, Partial.synonym} ->
            Just (position, synonym)
          _ -> Nothing

    annotation = case mapMaybe annotation (toList entries) of
      [] -> Strict.Nothing
      [(_, annotation)] -> Strict.Just annotation
      annotations -> duplicateTypeEntries (map fst annotations)
      where
        annotation = \case
          Partial.Annotation {Partial.position, Partial.annotation} -> Just (position, annotation)
          _ -> Nothing

indexes :: Strict.Vector (TypeDeclaration scope) -> Map ConstructorIdentifier Int
indexes types = Map.fromList $ zip [name typex | typex <- toList types] [0 ..]

bindings ::
  (Int -> Type.Index scope) ->
  Strict.Vector (TypeDeclaration scope) ->
  Map ConstructorIdentifier (Type.Binding scope)
bindings index types = Map.map typeIndex (indexes types)
  where
    typeIndex vectorIndex =
      Type.Binding
        { Type.position,
          Type.index = Type3.Index $ Type2.Index $ index vectorIndex,
          Type.constructors =
            case constructors of
              ADT constructors ->
                Set.fromList
                  [name | Constructor {Constructor.name} <- toList constructors]
              GADT gadtConstructors ->
                Set.fromList
                  [name | GADTConstructor {GADTConstructor.name} <- toList gadtConstructors]
              NoConstructors -> Set.empty,
          Type.fields =
            case fields of
              Selectors selectors -> Set.fromList [name | Selector {Selector.name} <- toList selectors]
              Methods methods -> Set.fromList [name | Method {Method.name} <- toList methods]
              NoFields -> Set.empty,
          Type.methods =
            case fields of
              Methods methods -> Map.fromList (zipWith (\(Method {Method.name}) i -> (name, i)) (toList methods) [0 ..])
              _ -> Map.empty
        }
      where
        TypeDeclaration {position, fields, constructors} =
          types Strict.Vector.! vectorIndex
