{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Temporary.Partial.Declaration
  ( Declaration (..),
    Key (..),
    resolve,
  )
where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error (missingMethodEntry)
import qualified Semantic.Index.Term as Term
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Pattern as Pattern
import qualified Semantic.Resolve.Go.RightHandSide as RightHandSide
import qualified Semantic.Resolve.Go.Scheme as Scheme
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.Complete.Declaration as Complete
  ( Declaration (Declaration, annotation),
  )
import qualified Semantic.Resolve.Temporary.Complete.Definition as Complete (Definition (..), resolve)
import qualified Semantic.Resolve.Temporary.Complete.Method as Complete (Method (Method))
import qualified Semantic.Resolve.Temporary.Complete.Method as Complete.Method
import qualified Semantic.Resolve.Temporary.Complete.Selector as Complete (Selector (Selector))
import qualified Semantic.Resolve.Temporary.Complete.Selector as Complete.Selector
import qualified Semantic.Resolve.Temporary.Complete.TypeDeclaration as Complete (TypeDeclaration (TypeDeclaration))
import qualified Semantic.Resolve.Temporary.Complete.TypeDeclaration as Complete.TypeDeclaration
import qualified Semantic.Resolve.Temporary.Partial.More.Choice as More (Choice (Choice))
import qualified Semantic.Resolve.Temporary.Partial.More.Choice as More.Choice
import qualified Semantic.Resolve.Temporary.Partial.More.Function as More (Function (Function))
import qualified Semantic.Resolve.Temporary.Partial.More.Function as More.Function
import qualified Semantic.Resolve.Temporary.Partial.More.Method as More (Method (Method))
import qualified Semantic.Resolve.Temporary.Partial.More.Method as More.Method
import qualified Semantic.Resolve.Temporary.Partial.More.Selector as More (Selector (Selector))
import qualified Semantic.Resolve.Temporary.Partial.More.Selector as More.Selector
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Combinators.Implicit as Real (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Inferred))
import Semantic.Tree.Declaration (Key (..))
import qualified Semantic.Tree.Declaration as Real (Declaration (..))
import qualified Semantic.Tree.Definition2 as Real (Definition2 (Shared))
import qualified Semantic.Tree.Definition3 as Real (Definition3 (..), Info (..))
import qualified Semantic.Tree.Definition4 as Real (Annotation (..), Definition4 (..))
import Semantic.Tree.Scheme (Scheme)
import Syntax.FreeVariables (TermBindingVariables (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.ClassDeclaration as Syntax.ClassDefinition
import qualified Syntax.Tree.ClassDeclarations as Syntax (ClassDeclarations (..))
import qualified Syntax.Tree.Declaration as Syntax (Declaration (..))
import Syntax.Tree.Fixity (Fixity)
import qualified Syntax.Tree.LeftHandSide as Syntax (LeftHandSide (Pattern))
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Pattern as Syntax (Pattern (Variable))
import qualified Syntax.Tree.RightHandSide as Syntax.RightHandSide
import Syntax.Variable (ConstructorIdentifier, Name (..), Variable)

data Declaration scope
  = Fixity
      { position :: !Position,
        name :: !Key,
        fixity :: !Fixity
      }
  | Annotation
      { position :: !Position,
        name :: !Key,
        annotation :: Scheme Position Resolve scope
      }
  | Function
      { position :: !Position,
        name :: !Key,
        function :: !(More.Function scope)
      }
  | Choice
      { position :: !Position,
        name :: !Key,
        choice :: !(More.Choice scope)
      }
  | Method
      { position :: !Position,
        name :: !Key,
        method :: !More.Method
      }
  | Selector
      { position :: !Position,
        name :: !Key,
        selector :: !More.Selector
      }
  | Shared
      { position :: !Position,
        name :: !Key,
        shared :: forall locality. Real.Declaration locality Normal Resolve scope
      }

resolve ::
  Context scope ->
  (Variable -> Complete.Declaration scope) ->
  (ConstructorIdentifier -> (Int, Complete.TypeDeclaration.TypeDeclaration scope)) ->
  (Int -> Term.Index scope) ->
  [Syntax.Declaration Position] ->
  [(Key, Declaration scope)]
resolve = resolve' 0

resolve' ::
  Int ->
  Context scope ->
  (Variable -> Complete.Declaration scope) ->
  (ConstructorIdentifier -> (Int, Complete.TypeDeclaration scope)) ->
  (Int -> Term.Index scope) ->
  [Syntax.Declaration Position] ->
  [(Key, Declaration scope)]
resolve'
  temporary
  context
  lookupTerm
  lookupType
  lookupShare
  ( Syntax.Definition
      { leftHandSide = Syntax.Pattern pattern1,
        rightHandSide
      }
      : declarations
    )
    | valid = entry ++ [(Unnamed temporary, shared)] ++ after
    where
      after = resolve' (temporary + 1) context lookupTerm lookupType lookupShare declarations
      valid = case pattern1 of
        Syntax.Variable {} -> False
        _ -> True
      patternx = Pattern.resolve context pattern1
      shared =
        Shared
          { position = Syntax.RightHandSide.equalPosition rightHandSide,
            name = Unnamed temporary,
            shared =
              Real.Declaration
                { position = Syntax.RightHandSide.equalPosition rightHandSide,
                  name = Unnamed temporary,
                  definition =
                    Real.Inferred
                      Real.::: Real.Resolve
                        ( Real.Unnamed temporary
                            `Real.Label` Real.Shared
                              (RightHandSide.resolve context rightHandSide)
                        ),
                  typex = Inferred
                }
          }
      entry = do
        let selections = Pattern.selections patternx
        (position :@ name) <- termBindingVariables pattern1
        let _ :@ bound = selections Map.! name
        pure
          ( Named name,
            Choice
              { position,
                name = Named name,
                choice =
                  More.Choice
                    { temporary,
                      index = lookupShare temporary,
                      bound,
                      patternx
                    }
              }
          )
resolve' temporary context lookupTerm lookupType lookupShare (declaration : declarations) =
  main ++ resolve' temporary context lookupTerm lookupType lookupShare declarations
  where
    main = case declaration of
      Syntax.Infix {fixity, termNames'} -> do
        position :@ Variable name <- toList termNames'
        pure (Named name, Fixity {position, name = Named name, fixity})
      Syntax.Annotation {termNames, annotation} -> do
        position :@ name <- toList termNames
        annotation <- pure $ Scheme.resolve context annotation
        pure (Named name, Annotation {position, name = Named name, annotation})
      Syntax.Definition {leftHandSide, rightHandSide} ->
        let Complete.Definition position name functionAuto =
              Complete.resolve undefined context leftHandSide rightHandSide
            functionManual = case lookupTerm name of
              Complete.Declaration {annotation = Just annotation} ->
                let context' = Scheme.augment annotation context
                    Complete.Definition _ _ functionManual =
                      Complete.resolve undefined context' leftHandSide rightHandSide
                 in functionManual
              _ -> error "manual scheme without annotation"
         in [ ( Named name,
                Function
                  { position,
                    name = Named name,
                    function =
                      More.Function
                        { functionAuto,
                          functionManual
                        }
                  }
              )
            ]
      Syntax.Data {typeName = name}
        | (typeIndex, declaration) <- lookupType name -> case declaration of
            Complete.TypeDeclaration {fields}
              | Complete.TypeDeclaration.Selectors selectors <- fields ->
                  zipWith entry [0 ..] $ toList selectors
              | otherwise -> error "data with no selectors"
              where
                entry selectorIndex Complete.Selector {name, position} =
                  ( Named name,
                    Selector
                      { position,
                        name = Named name,
                        selector =
                          More.Selector
                            { typeIndex,
                              selectorIndex
                            }
                      }
                  )
      Syntax.Class {typeName = name, classDefinition}
        | (typeIndex, declaration) <- lookupType name -> case declaration of
            Complete.TypeDeclaration {fields}
              | Complete.TypeDeclaration.Methods methods <- fields ->
                  let Syntax.ClassDeclarations {declarations} = classDefinition
                      methodSet = Set.fromList [name | Complete.Method {name} <- toList methods]
                      entry methodIndex Complete.Method {name, position} =
                        ( Named name,
                          Method
                            { position,
                              name = Named name,
                              method =
                                More.Method
                                  { typeIndex,
                                    methodIndex
                                  }
                            }
                        )

                      fixities
                        Syntax.ClassDefinition.Infix
                          { fixity,
                            termNames'
                          } = do
                          position :@ name <- toList termNames'
                          if
                            | Variable name <- name,
                              name `Set.member` methodSet ->
                                pure (Named name, Fixity {position, name = Named name, fixity})
                            | otherwise -> missingMethodEntry position
                      fixities _ = []
                   in zipWith entry [0 ..] (toList methods) ++ foldMap fixities declarations
              | otherwise -> error "class with no methods"
      _ -> []
resolve' _ _ _ _ _ [] = []
