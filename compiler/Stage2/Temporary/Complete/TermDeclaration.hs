module Stage2.Temporary.Complete.TermDeclaration where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Error
  ( duplicateAnnotationEntries,
    duplicateFieldEntries,
    duplicateFixityEntries,
    duplicateMethodEntries,
    duplicateVariableEntries,
    missingVariableEntry,
  )
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable)
import qualified Stage1.Variable as Variable
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector (Index (..))
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Detail.Binding.Term as Selector (Selector (..))
import qualified Stage2.Temporary.Partial.More.Function as More.Function
import qualified Stage2.Temporary.Partial.More.Method as More (Method (Method))
import qualified Stage2.Temporary.Partial.More.Method as More.Method
import qualified Stage2.Temporary.Partial.More.Selector as More (Selector (Selector))
import qualified Stage2.Temporary.Partial.More.Selector as More.Selector
import qualified Stage2.Temporary.Partial.More.Share as More (Shared (Shared))
import qualified Stage2.Temporary.Partial.More.Share as More.Share
import qualified Stage2.Temporary.Partial.TermDeclaration as Partial
import qualified Stage2.Tree.Definition as Definition (merge)
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.TermDeclaration as Real
import Verbose (Debug (resolving))
import Prelude hiding (Either (Left, Right), Real)

data Real scope
  = Real (Real.TermDeclaration scope)
  | Select !More.Selector
  | Method !More.Method

data TermDeclaration scope
  = TermDeclaration
  { position :: !Position,
    name :: !Variable,
    fixity :: !Fixity,
    annotation :: !(Maybe (Scheme Position scope)),
    declaration :: !(Real scope)
  }

shrink :: TermDeclaration scope -> Maybe (Real.TermDeclaration scope)
shrink TermDeclaration {declaration} = case declaration of
  Real valid -> Just valid
  _ -> Nothing

merge :: (Debug verbose) => NonEmpty (Partial.TermDeclaration scope) -> verbose (TermDeclaration scope)
merge entries@(entry :| _) =
  let termDeclaration declaration = TermDeclaration {position, name, fixity, annotation, declaration}
   in termDeclaration <$> declaration
  where
    declaration = case catMaybes [fmap fst functions, fmap fst selection, fmap fst method, fmap fst share] of
      [] -> missingVariableEntry position
      [_]
        | Just (position, body) <- functions -> case annotation of
            Nothing ->
              Real
                <$> Verbose.resolving
                  (Variable.printLiteral' name)
                  Real.Auto
                    { position,
                      name,
                      fixity,
                      definitionAuto = Definition.merge $ fmap More.Function.functionAuto body
                    }
            Just annotation ->
              Real
                <$> Verbose.resolving
                  (Variable.printLiteral' name)
                  Real.Manual
                    { position,
                      name,
                      fixity,
                      definition = Definition.merge $ fmap More.Function.functionManual body,
                      annotation
                    }
        | Just (_, More.Selector {typeIndex, selectorIndex}) <- selection,
          () <- noAnnotation ->
            pure $
              Select
                More.Selector
                  { typeIndex,
                    selectorIndex
                  }
        | Just (_, More.Method {typeIndex, methodIndex}) <- method,
          () <- noAnnotation ->
            pure $
              Method
                More.Method
                  { typeIndex,
                    methodIndex
                  }
        | Just
            ( position,
              More.Shared
                { shareIndex,
                  bound
                }
              ) <-
            share ->
            let annotationShare = maybe Strict.Nothing Strict.Just annotation
             in pure $
                  Real
                    Real.Share
                      { position,
                        name,
                        fixity,
                        shareIndex,
                        bound,
                        annotationShare
                      }
        | otherwise -> error "no entry"
      entries -> duplicateVariableEntries entries
    position = Partial.position entry
    name = Partial.name entry
    fixity = case mapMaybe fixity (toList entries) of
      [] -> Fixity {associativity = Left, precedence = 9}
      [(_, fixity)] -> fixity
      fixities -> duplicateFixityEntries (map fst fixities)
      where
        fixity = \case
          Partial.Fixity {position, fixity} -> Just (position, fixity)
          _ -> Nothing
    annotation = fmap snd annotation'
    annotation' = case mapMaybe annotation (toList entries) of
      [] -> Nothing
      [annotation] -> Just annotation
      annotations -> duplicateAnnotationEntries (map fst annotations)
      where
        annotation = \case
          Partial.Annotation {position, annotation} -> Just (position, annotation)
          _ -> Nothing
    functions = case mapMaybe definition (toList entries) of
      (position, function) : functions -> Just (position, function :| fmap snd functions)
      [] -> Nothing
      where
        definition = \case
          Partial.Function {position, function} ->
            Just (position, function)
          _ -> Nothing
    selection = case mapMaybe selector (toList entries) of
      [] -> Nothing
      [selector] -> Just selector
      selectors -> duplicateFieldEntries (map fst selectors)
      where
        selector = \case
          Partial.Selector {position, selector} ->
            Just (position, selector)
          _ -> Nothing
    method = case mapMaybe method (toList entries) of
      [] -> Nothing
      [method] -> Just method
      methods -> duplicateMethodEntries (map fst methods)
      where
        method = \case
          Partial.Method {position, method} ->
            Just (position, method)
          _ -> Nothing

    share = case mapMaybe shared (toList entries) of
      [] -> Nothing
      [share] -> Just share
      shared -> duplicateVariableEntries (map fst shared)
      where
        shared = \case
          Partial.Shared {position, share} ->
            Just (position, share)
          _ -> Nothing
    noAnnotation = case annotation' of
      Nothing -> ()
      Just (position', _) -> duplicateAnnotationEntries [position', position]

indexes :: Strict.Vector (TermDeclaration scope) -> Map Variable Int
indexes terms = Map.fromList $ zip (name <$> toList terms) [0 ..]

bindings ::
  (Int -> Term0.Index scope) ->
  (Int -> Type.Index scope) ->
  Strict.Vector (TermDeclaration scope) ->
  Map Variable (Term.Binding scope)
bindings index index' terms = Map.fromList $ makeIndexes 0 (toList terms)
  where
    makeIndexes n = \case
      [] -> []
      TermDeclaration {position, name, fixity, declaration} : declarations -> case declaration of
        Real _ ->
          (name, value) : makeIndexes (n + 1) declarations
          where
            value =
              Term.Binding
                { position,
                  index = Term2.Index $ Term0.normal $ index n,
                  fixity,
                  selector = Selector.Normal
                }
        Select More.Selector {typeIndex, selectorIndex} ->
          (name, value) : makeIndexes n declarations
          where
            select = Selector.Index (Type2.Index $ index' typeIndex) selectorIndex
            value =
              Term.Binding
                { position,
                  index = Term2.Select select,
                  fixity,
                  selector = Selector.Selector select
                }
        Method More.Method {typeIndex, methodIndex} ->
          (name, value) : makeIndexes n declarations
          where
            method = Method.Index (Type2.Index $ index' typeIndex) methodIndex
            value =
              Term.Binding
                { position,
                  index = Term2.Method method,
                  fixity,
                  selector = Selector.Normal
                }
