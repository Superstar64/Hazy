{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Partial.Declaration
  ( Declaration (..),
    Key (..),
    resolve,
  )
where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error (missingMethodEntry)
import Stage1.FreeVariables (TermBindingVariables (..))
import Stage1.Position (Position)
import qualified Stage1.Tree.ClassDeclaration as Stage1.ClassDefinition
import qualified Stage1.Tree.ClassDeclarations as Stage1 (ClassDeclarations (..))
import qualified Stage1.Tree.Declaration as Stage1 (Declaration (..))
import Stage1.Tree.Fixity (Fixity)
import qualified Stage1.Tree.LeftHandSide as Stage1 (LeftHandSide (Pattern))
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (Variable))
import qualified Stage1.Tree.RightHandSide as Stage1.RightHandSide
import Stage1.Variable (ConstructorIdentifier, Name (..), Variable)
import qualified Stage2.Index.Term as Term
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Stage (Resolve)
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.Declaration as Complete
  ( Declaration (Declaration, annotation),
  )
import qualified Stage2.Temporary.Complete.Definition as Complete (Definition (..), resolve)
import qualified Stage2.Temporary.Complete.Method as Complete (Method (Method))
import qualified Stage2.Temporary.Complete.Method as Complete.Method
import qualified Stage2.Temporary.Complete.Selector as Complete (Selector (Selector))
import qualified Stage2.Temporary.Complete.Selector as Complete.Selector
import qualified Stage2.Temporary.Complete.TypeDeclaration as Complete (TypeDeclaration (TypeDeclaration))
import qualified Stage2.Temporary.Complete.TypeDeclaration as Complete.TypeDeclaration
import qualified Stage2.Temporary.Partial.More.Choice as More (Choice (Choice))
import qualified Stage2.Temporary.Partial.More.Choice as More.Choice
import qualified Stage2.Temporary.Partial.More.Function as More (Function (Function))
import qualified Stage2.Temporary.Partial.More.Function as More.Function
import qualified Stage2.Temporary.Partial.More.Method as More (Method (Method))
import qualified Stage2.Temporary.Partial.More.Method as More.Method
import qualified Stage2.Temporary.Partial.More.Selector as More (Selector (Selector))
import qualified Stage2.Temporary.Partial.More.Selector as More.Selector
import qualified Stage2.Tree.Combinators.Implicit as Real (Implicit (..))
import Stage2.Tree.Declaration (Key (..))
import qualified Stage2.Tree.Declaration as Real (Declaration (..))
import qualified Stage2.Tree.Definition2 as Real (Definition2 (Shared))
import qualified Stage2.Tree.Definition3 as Real (Definition3 (..), Info (..))
import qualified Stage2.Tree.Definition4 as Real (Annotation (..), Definition4 (..))
import qualified Stage2.Tree.Pattern as Pattern
import qualified Stage2.Tree.RightHandSide as RightHandSide
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Scheme

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
  [Stage1.Declaration Position] ->
  [(Key, Declaration scope)]
resolve = resolve' 0

resolve' ::
  Int ->
  Context scope ->
  (Variable -> Complete.Declaration scope) ->
  (ConstructorIdentifier -> (Int, Complete.TypeDeclaration scope)) ->
  (Int -> Term.Index scope) ->
  [Stage1.Declaration Position] ->
  [(Key, Declaration scope)]
resolve'
  temporary
  context
  lookupTerm
  lookupType
  lookupShare
  ( Stage1.Definition
      { leftHandSide = Stage1.Pattern pattern1,
        rightHandSide
      }
      : declarations
    )
    | valid = entry ++ [(Unnamed temporary, shared)] ++ after
    where
      after = resolve' (temporary + 1) context lookupTerm lookupType lookupShare declarations
      valid = case pattern1 of
        Stage1.Variable {} -> False
        _ -> True
      patternx = Pattern.resolve context pattern1
      shared =
        Shared
          { position = Stage1.RightHandSide.equalPosition rightHandSide,
            name = Unnamed temporary,
            shared =
              Real.Declaration
                { position = Stage1.RightHandSide.equalPosition rightHandSide,
                  name = Unnamed temporary,
                  definition =
                    Real.Inferred
                      Real.::: Real.Unnamed temporary
                      Real.::@ Real.Resolve
                        (Real.Shared $ RightHandSide.resolve context rightHandSide)
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
      Stage1.Infix {fixity, termNames'} -> do
        position :@ Variable name <- toList termNames'
        pure (Named name, Fixity {position, name = Named name, fixity})
      Stage1.Annotation {termNames, annotation} -> do
        position :@ name <- toList termNames
        annotation <- pure $ Scheme.resolve context annotation
        pure (Named name, Annotation {position, name = Named name, annotation})
      Stage1.Definition {leftHandSide, rightHandSide} ->
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
      Stage1.Data {typeName = name}
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
      Stage1.Class {typeName = name, classDefinition}
        | (typeIndex, declaration) <- lookupType name -> case declaration of
            Complete.TypeDeclaration {fields}
              | Complete.TypeDeclaration.Methods methods <- fields ->
                  let Stage1.ClassDeclarations {declarations} = classDefinition
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
                        Stage1.ClassDefinition.Infix
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
