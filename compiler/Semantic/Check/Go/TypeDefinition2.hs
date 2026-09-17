module Semantic.Check.Go.TypeDefinition2 where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Type (simplify)
import Core.Type.Functor (shiftLogical)
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as Vector
import Data.Vector.Strict (toLazy)
import qualified Data.Vector.Strict as Strict.Vector
import Error (cyclicalTypeChecking)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context, groupTypeBindings)
import qualified Semantic.Check.Go.Synonym as Synonym
import qualified Semantic.Check.Temporary.Type as Type (check, solve)
import qualified Semantic.Check.Temporary.TypeDefinition as TypeDefinition
import qualified Semantic.Layout as Layout
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.TypeDefinition2 (Annotation (..), TypeDefinition2 (..))
import Semantic.Tree.TypeGroup (Element (..), Set (..), TypeGroup (..), Types (..))
import qualified Semantic.Tree.TypeGroup as TypeGroup
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

check ::
  Position ->
  (declarations (ST s) -> TypeDefinition2 locality (ST s) Layout.Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  TypeDefinition2 locality Identity Layout.Group Resolve scope ->
  TypeDefinition2 locality (Topological.Formula declarations s) Layout.Group Check scope
check position reflection information = \case
  Annotated (Identity annotation) ::: Identity definition ->
    Annotated
      Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
            typex <- Type.check context Core.kind annotation
            typex <- Unify.runSolve $ Type.solve context typex
            pure typex
        }
      ::: Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
                self = reflection declarations
            case self of
              Annotated annotation ::: _ -> do
                typex <- annotation
                typex <- pure $ logicalType $ Type.simplify typex
                definition <- TypeDefinition.check context typex definition
                definition <- Unify.runSolve $ TypeDefinition.solve context definition
                pure definition
              _ -> error "bad self definition"
        }
  Link link id -> Link link id
  Group (Identity (_ :::: Set set)) ->
    Group
      Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
            fresh <- Vector.replicateM (length set) (Unify.fresh Core.kind)
            let context' =
                  groupTypeBindings
                    (TypeGroup.position <$> toLazy set)
                    (TypeGroup.label <$> toLazy set)
                    fresh
                    context
            set <-
              flip Strict.Vector.imapM set $
                \index
                 Element {element, position, name, constructorNames, link} -> do
                    let typex = fresh Vector.! index
                    element <- TypeDefinition.check context' (shiftLogical typex) element
                    pure $ do
                      element <- TypeDefinition.solve context' element
                      typex <- Unify.solve position typex
                      pure Element {element, typex = Solved typex, position, name, constructorNames, link}
            set <- Unify.runSolve $ sequence set
            kinds <- Unify.runSolve $ traverse (Unify.solve position) fresh
            pure $ Solved (Types $ Strict.Vector.fromLazy kinds) :::: Set set
        }
  Synonym (Identity synonym) ->
    Synonym
      Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
            synonym <- Synonym.check context position synonym
            pure synonym
        }
