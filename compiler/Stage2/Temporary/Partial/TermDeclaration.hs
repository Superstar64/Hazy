{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Temporary.Partial.TermDeclaration where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Error (missingMethodEntry)
import Stage1.Position (Position)
import qualified Stage1.Tree.ClassDeclaration as Stage1.ClassDefinition
import qualified Stage1.Tree.ClassDeclarations as Stage1 (ClassDeclarations (ClassDeclarations))
import qualified Stage1.Tree.Declaration as Stage1 (Declaration (..))
import Stage1.Tree.Fixity (Fixity)
import qualified Stage1.Tree.LeftHandSide as Stage1 (LeftHandSide (Pattern))
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (Variable))
import Stage1.Variable (ConstructorIdentifier, Name (..), Variable)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Temporary.Complete.Method as Complete (Method (Method))
import qualified Stage2.Temporary.Complete.Method as Complete.Method
import qualified Stage2.Temporary.Complete.Selector as Complete (Selector (Selector))
import qualified Stage2.Temporary.Complete.Selector as Complete.Selector
import qualified Stage2.Temporary.Complete.Shared as Complete (Shared (Shared))
import qualified Stage2.Temporary.Complete.Shared as Complete.Shared (Shared (..))
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.TermDeclaration as Complete
  ( TermDeclaration (TermDeclaration, annotation),
  )
import qualified Stage2.Temporary.Complete.TypeDeclaration as Complete (TypeDeclaration (TypeDeclaration))
import qualified Stage2.Temporary.Complete.TypeDeclaration as Complete.TypeDeclaration
import qualified Stage2.Temporary.Partial.Definition as Partial
import qualified Stage2.Temporary.Partial.More.Function as More (Function (Function))
import qualified Stage2.Temporary.Partial.More.Function as More.Function
import qualified Stage2.Temporary.Partial.More.Method as More (Method (Method))
import qualified Stage2.Temporary.Partial.More.Method as More.Method
import qualified Stage2.Temporary.Partial.More.Selector as More (Selector (Selector))
import qualified Stage2.Temporary.Partial.More.Selector as More.Selector
import qualified Stage2.Temporary.Partial.More.Share as More (Shared (Shared))
import qualified Stage2.Temporary.Partial.More.Share as More.Shared
import qualified Stage2.Tree.Pattern as Pattern
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Scheme
import qualified Stage2.Tree.Shared as Shared

data TermDeclaration scope
  = Fixity
      { position :: !Position,
        name :: !Variable,
        fixity :: !Fixity
      }
  | Annotation
      { position :: !Position,
        name :: !Variable,
        annotation :: Scheme Position scope
      }
  | Function
      { position :: !Position,
        name :: !Variable,
        function :: !(More.Function scope)
      }
  | Shared
      { position :: !Position,
        name :: !Variable,
        share :: !More.Shared
      }
  | Method
      { position :: !Position,
        name :: !Variable,
        method :: !More.Method
      }
  | Selector
      { position :: !Position,
        name :: !Variable,
        selector :: !More.Selector
      }

resolve ::
  Context scope ->
  (Variable -> Complete.TermDeclaration scope) ->
  (ConstructorIdentifier -> (Int, Complete.TypeDeclaration scope)) ->
  (Int -> Complete.Shared scope) ->
  Int ->
  [Stage1.Declaration Position] ->
  [(Variable, TermDeclaration scope)]
resolve
  context
  lookupTerm
  lookupType
  lookupShared
  shareIndex
  ( Stage1.Definition
      { Stage1.leftHandSide =
          Stage1.Pattern pattern1
      }
      : declarations
    )
    | valid = entry ++ resolve context lookupTerm lookupType lookupShared (shareIndex + 1) declarations
    where
      valid = case pattern1 of
        Stage1.Variable {} -> False
        _ -> True
      entry = do
        let Shared.Shared {Shared.patternx} = share
            selections = Pattern.selections patternx
        (position :@ name) <- toList bindings
        let _ :@ bound = selections Map.! name
        pure
          ( name,
            Shared
              { position,
                name,
                share =
                  More.Shared
                    { More.Shared.shareIndex,
                      More.Shared.bound
                    }
              }
          )
      Complete.Shared {Complete.Shared.bindings, Complete.Shared.share} =
        lookupShared shareIndex
resolve context lookupTerm lookupType lookupShared shareIndex (declaration : declarations) =
  main ++ resolve context lookupTerm lookupType lookupShared shareIndex declarations
  where
    main = case declaration of
      Stage1.Infix {Stage1.fixity, Stage1.termNames'} -> do
        position :@ Variable name <- toList termNames'
        pure (name, Fixity {position, name, fixity})
      Stage1.Annotation {Stage1.termNames, Stage1.annotation} -> do
        position :@ name <- toList termNames
        annotation <- pure $ Scheme.resolve context annotation
        pure (name, Annotation {position, name, annotation})
      Stage1.Definition {Stage1.leftHandSide, Stage1.rightHandSide} ->
        let Partial.Definition position name functionAuto = Partial.resolve undefined context leftHandSide rightHandSide
            functionManual = case lookupTerm name of
              Complete.TermDeclaration {Complete.annotation = Just annotation} ->
                let context' = Scheme.augment annotation context
                    Partial.Definition _ _ functionManual =
                      Partial.resolve undefined context' leftHandSide rightHandSide
                 in functionManual
              _ -> error "manual scheme without annotation"
         in [ ( name,
                Function
                  { position,
                    name,
                    function =
                      More.Function
                        { More.Function.functionAuto,
                          More.Function.functionManual
                        }
                  }
              )
            ]
      Stage1.Data {Stage1.typeName = name}
        | (typeIndex, declaration) <- lookupType name -> case declaration of
            Complete.TypeDeclaration {Complete.TypeDeclaration.fields}
              | Complete.TypeDeclaration.Selectors selectors <- fields ->
                  zipWith entry [0 ..] $ toList selectors
              | otherwise -> error "data with no selectors"
              where
                entry selectorIndex Complete.Selector {Complete.Selector.name, Complete.Selector.position} =
                  ( name,
                    Selector
                      { position,
                        name,
                        selector =
                          More.Selector
                            { More.Selector.typeIndex,
                              More.Selector.selectorIndex
                            }
                      }
                  )
      Stage1.Class {Stage1.typeName = name, Stage1.classDefinition}
        | (typeIndex, declaration) <- lookupType name -> case declaration of
            Complete.TypeDeclaration {Complete.TypeDeclaration.fields}
              | Complete.TypeDeclaration.Methods methods <- fields ->
                  let Stage1.ClassDeclarations declarations = classDefinition
                      methodSet = Set.fromList [name | Complete.Method {Complete.Method.name} <- toList methods]
                      entry methodIndex Complete.Method {Complete.Method.name, Complete.Method.position} =
                        ( name,
                          Method
                            { position,
                              name,
                              method =
                                More.Method
                                  { More.Method.typeIndex,
                                    More.Method.methodIndex
                                  }
                            }
                        )

                      fixities
                        Stage1.ClassDefinition.Infix
                          { Stage1.ClassDefinition.fixity,
                            Stage1.ClassDefinition.termNames'
                          } = do
                          position :@ name <- toList termNames'
                          if
                            | Variable name <- name,
                              name `Set.member` methodSet ->
                                pure (name, Fixity {position, name, fixity})
                            | otherwise -> missingMethodEntry position
                      fixities _ = []
                   in zipWith entry [0 ..] (toList methods) ++ foldMap fixities declarations
              | otherwise -> error "class with no methods"
      _ -> []
resolve _ _ _ _ _ [] = []
