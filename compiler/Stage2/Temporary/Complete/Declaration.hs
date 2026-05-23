module Stage2.Temporary.Complete.Declaration where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
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
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Normal)
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Detail.Binding.Term as Selector (Selector (..))
import Stage2.Stage (Resolve)
import Stage2.Temporary.Partial.Declaration (Key (..))
import qualified Stage2.Temporary.Partial.Declaration as Partial
import qualified Stage2.Temporary.Partial.More.Choice as More (Choice (Choice))
import qualified Stage2.Temporary.Partial.More.Choice as More.Choice
import qualified Stage2.Temporary.Partial.More.Function as More.Function
import qualified Stage2.Temporary.Partial.More.Method as More (Method (Method))
import qualified Stage2.Temporary.Partial.More.Method as More.Method
import qualified Stage2.Temporary.Partial.More.Selector as More (Selector (Selector))
import qualified Stage2.Temporary.Partial.More.Selector as More.Selector
import qualified Stage2.Tree.Combinators.Implicit as Real (Implicit (..))
import Stage2.Tree.Combinators.Inferred (Inferred (Inferred))
import qualified Stage2.Tree.Declaration as Real (Declaration (..), locality)
import qualified Stage2.Tree.Definition as Definition (merge)
import qualified Stage2.Tree.Definition2 as Real (Choice (..), Definition2 (..))
import qualified Stage2.Tree.Definition3 as Real (Definition3 (..), Info (..))
import qualified Stage2.Tree.Definition4 as Real (Annotation (..), Definition4 (..))
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Scheme
import Verbose (Debug (resolving))
import Prelude hiding (Either (Left, Right), Real)

data Real scope
  = Real (forall locality. Real.Declaration locality Normal Resolve scope)
  | Select !More.Selector
  | Method !More.Method

data Declaration scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    fixity :: !Fixity,
    annotation :: !(Maybe (Scheme Position Resolve scope)),
    declaration :: !(Real scope)
  }

shrink :: Declaration scope -> Maybe (Real.Declaration locality Normal Resolve scope)
shrink Declaration {declaration} = case declaration of
  Real valid -> Just valid
  _ -> Nothing

merge ::
  forall scope verbose.
  (Debug verbose) =>
  NonEmpty (Partial.Declaration scope) ->
  verbose (Declaration scope)
merge entries@(entry :| _) =
  let termDeclaration declaration = Declaration {position, name, fixity, annotation, declaration}
   in termDeclaration <$> declaration
  where
    declaration = case catMaybes
      [ fmap fst functions,
        fmap fst selection,
        fmap fst method,
        fmap fst choice
      ] of
      _ | Partial.Shared {shared} :| [] <- entries -> pure $ Real shared
      [] -> missingVariableEntry position
      [_]
        | Just (position, body) <- functions ->
            let manual = Definition.merge $ fmap More.Function.functionManual body
                auto = Definition.merge $ fmap More.Function.functionAuto body
             in case annotation of
                  Nothing -> cast <$> Verbose.resolving (Variable.printLiteral' properName) real
                    where
                      cast declaration = Real $ Real.locality declaration
                      real =
                        Real.Declaration
                          { position,
                            name,
                            definition =
                              Real.Inferred
                                Real.::: Real.Name properName fixity
                                Real.::@ Real.Resolve (Real.Definition auto),
                            typex = Inferred
                          }
                  Just annotation -> cast <$> Verbose.resolving (Variable.printLiteral' properName) real
                    where
                      cast declaration = Real $ Real.locality declaration
                      real =
                        Real.Declaration
                          { position,
                            name,
                            definition =
                              Real.Annotated annotation
                                Real.::: Real.Name properName fixity
                                Real.::@ if Scheme.implicit annotation
                                  then Real.Resolve (Real.Definition auto)
                                  else Real.Resolve (Real.Scoped manual),
                            typex = Inferred
                          }
        | Just (_, selector) <- selection,
          () <- noAnnotation ->
            pure $ Select selector
        | Just (_, method) <- method,
          () <- noAnnotation ->
            pure $ Method method
        | Just (position, More.Choice {index, bound, patternx}) <- choice ->
            let cast declaration = Real $ Real.locality declaration
                real = case annotation of
                  Nothing ->
                    Real.Declaration
                      { position,
                        name,
                        definition =
                          Real.Inferred
                            Real.::: Real.Name properName fixity
                            Real.::@ Real.Resolve
                              (Real.Piece Real.Choice {position, index, bound, patternx}),
                        typex = Inferred
                      }
                  Just annotation ->
                    Real.Declaration
                      { position,
                        name,
                        definition =
                          Real.Annotated annotation
                            Real.::: Real.Name properName fixity
                            Real.::@ Real.Resolve
                              (Real.Piece Real.Choice {position, index, bound, patternx}),
                        typex = Inferred
                      }
             in cast <$> Verbose.resolving (Variable.printLiteral' properName) real
        | otherwise -> error "no entry"
        where
          properName = case name of
            Named name -> name
            Unnamed _ -> error "bad name"
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

    choice = case mapMaybe choice (toList entries) of
      [] -> Nothing
      [choice] -> Just choice
      choice -> duplicateVariableEntries (map fst choice)
      where
        choice = \case
          Partial.Choice {position, choice} ->
            Just (position, choice)
          _ -> Nothing

    noAnnotation = case annotation' of
      Nothing -> ()
      Just (position', _) -> duplicateAnnotationEntries [position', position]

indexes :: Strict.Vector (Declaration scope) -> Map Variable Int
indexes terms = Map.fromList [(name, index) | (Named name, index) <- zip (name <$> toList terms) [0 ..]]

bindings ::
  (Int -> Term0.Index scope) ->
  (Int -> Type0.Index scope) ->
  Strict.Vector (Declaration scope) ->
  Map Variable (Term.Binding scope)
bindings index index' terms =
  Map.fromList $ [(name, binding) | (Named name, binding) <- makeIndexes 0 (toList terms)]
  where
    makeIndexes n = \case
      [] -> []
      Declaration {position, name, fixity, declaration} : declarations -> case declaration of
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
            select =
              Selector.Index
                { typeIndex = Type2.Index $ Type0.normal $ index' typeIndex,
                  selectorIndex
                }
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
            method =
              Method.Index
                { typeIndex = Type2.Index $ Type0.normal $ index' typeIndex,
                  methodIndex
                }
            value =
              Term.Binding
                { position,
                  index = Term2.Method method,
                  fixity,
                  selector = Selector.Normal
                }
