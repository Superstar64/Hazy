module Semantic.Check.Go.TypeDeclaration where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Type (TypeF, simplify)
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cyclicalTypeChecking)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Go.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Index.Link.Type as Type (Link)
import Semantic.Layout (Group)
import qualified Semantic.Layout as Layout
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Stage as Stage
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Synonym (SynonymBody (..))
import qualified Semantic.Tree.Synonym as Synonym
import Semantic.Tree.TypeDeclaration (TypeDeclaration (..))
import Semantic.Tree.TypeDefinition2 (Annotation (..), TypeDefinition2 (..))
import Semantic.Tree.TypeGroup (TypeGroup (..), Types (..))

check ::
  Type.Link locality ->
  ( declarations (ST s) ->
    Type.Link locality ->
    TypeDeclaration locality (ST s) Group Check scope
  ) ->
  (declarations (ST s) -> Context s scope) ->
  TypeDeclaration locality Identity Group Resolve scope ->
  TypeDeclaration locality (Topological.Formula declarations s) Group Check scope
check
  link
  reflection
  information
  TypeDeclaration {position, name, constructorNames, definition} =
    TypeDeclaration
      { position,
        name,
        constructorNames,
        definition =
          let reflection' declarations = case reflection declarations link of
                TypeDeclaration {definition} -> definition
           in TypeDefinition2.check position reflection' information definition,
        kind =
          Topological.Formula
            { cycle = cyclicalTypeChecking position,
              run = \declarations -> do
                let TypeDeclaration {definition} = reflection declarations link
                    inferred ::
                      Int ->
                      TypeGroup locality Layout.Group Stage.Check scope ->
                      Inferred (Type.TypeF Void) Stage.Check scope
                    inferred index (Solved (Types types) :::: _) = Solved $ types Strict.Vector.! index
                case definition of
                  Annotated annotation ::: _ -> do
                    typex <- annotation
                    pure $ Solved $ Type.simplify typex
                  Link link id -> do
                    let TypeDeclaration {definition} = reflection declarations link
                    case definition of
                      Group group -> inferred id <$> group
                      _ -> error "bad self type declaration"
                  Group group -> inferred 0 <$> group
                  Synonym synonym -> do
                    _ Synonym.::: SynonymBody {kind} <- synonym
                    pure $ kind
            }
      }
