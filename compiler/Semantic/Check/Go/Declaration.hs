module Semantic.Check.Go.Declaration where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType)
import qualified Core.Tree.Constraints as Simple.Constraints (simplify)
import qualified Core.Tree.Forall as Forall
import qualified Core.Tree.Type as Core
import qualified Core.Tree.Type as Simple (simplify)
import qualified Core.Tree.TypeLambda as Simple (TypeLambdaOver (..))
import Core.Type.Functor (shiftLogical)
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Semantic.Check.Context (Context (..), groupTermBindings)
import qualified Semantic.Check.Go.Scheme as Solved.Scheme
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Temporary.Definition3 as Definition3
import Semantic.Check.TypeAnnotation (TypeAnnotation (..))
import qualified Semantic.Index.Link.Term as Term
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Category (Shift))
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import Semantic.Tree.Declaration (Declaration (..))
import Semantic.Tree.Definition4 (Definition4 (..))
import qualified Semantic.Tree.Definition4 as Definition4
import qualified Semantic.Tree.Definition4 as Semantic
  ( Annotation (..),
    Definition4 (..),
  )
import Semantic.Tree.Group (Element (..), Set (..), Types (..))
import qualified Semantic.Tree.Group as Semantic (Group (..))
import qualified Semantic.Tree.Group as Solved (Set (..))
import Semantic.Tree.Scheme as Solved (Scheme (..))
import qualified Semantic.Tree.TypePattern as TypePattern
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

check ::
  Context s scope ->
  (Term.Link locality -> Int -> ST s (Unify.Forall s scope)) ->
  TypeAnnotation scope ->
  Declaration Identity Void locality Identity Group Resolve scope ->
  ST s (Declaration (Unify.Solve s) (Unify.Logical s scope) locality Identity Group Check scope)
check context linked annotation Declaration {position, name, definition} = case definition of
  Semantic.Annotated {} Semantic.::: Identity (Identity (Implicit.Resolve definition))
    | Annotated annotation <- annotation -> do
        let annotation' = Forall.simplify annotation
        definition <- checkAnnotation context position annotation $ \context typex -> do
          definition <- Definition3.checkManual context typex definition
          pure $ Definition3.solve position definition
        pure $
          Declaration
            { position,
              name,
              definition = Semantic.Annotated (Identity annotation) ::: Identity (fmap Implicit.Check definition),
              typex = Identity (Inferred.Solved $ logicalType annotation')
            }
    | otherwise -> error "bad type annotation"
  Semantic.Group (Identity (_ Semantic.:::: Identity (Implicit.Resolve (Set set)))) -> do
    types Unify.::: set <- Unify.generalizeBody position context $ Unify.Generalize $ \context -> do
      fresh <- Vector.replicateM (length set) $ Unify.fresh Core.typex
      set <- flip Strict.Vector.imapM set $ \index Element {element, link} -> do
        let element' = Shift.map (Shift.Over Shift) element
            typex = fresh Vector.! index
        element <- Definition3.checkAuto (groupTermBindings fresh context) (shiftLogical typex) element'
        pure $ do
          element <- Definition3.solve position element
          pure Element {element, link}
      let types = Types (Strict.Vector.fromLazy fresh)
          solved = do
            set <- sequence set
            pure $ Solved.Set set
      pure $ types Unify.::: solved
    let initial = Unify.MapForall $ \(Types types) -> Strict.Vector.head types
    pure
      Declaration
        { position,
          name,
          definition = Semantic.Group $ Identity $ Inferred.Solved types Semantic.:::: fmap Implicit.Check set,
          typex = Identity (Inferred.Solved $ Unify.mapForall initial types)
        }
  Semantic.Link link id -> do
    typex <- linked link id
    pure
      Declaration
        { position,
          name,
          definition = Link link id,
          typex = Identity (Inferred.Solved typex)
        }

checkAnnotation ::
  Context s scope ->
  Position ->
  Solved.Scheme Position Check scope ->
  ( Context s (Local ':+ scope) ->
    Unify.Type s (Local ':+ scope) ->
    ST s (Unify.Solve s (typex (Local ':+ scope)))
  ) ->
  ST s (Unify.Solve s (Simple.TypeLambdaOver typex scope))
checkAnnotation
  context
  position
  Solved.Scheme
    { parameters,
      constraints,
      result
    }
  go =
    do
      let typex = logicalType $ Simple.simplify result
      context <- Solved.Scheme.augment position parameters constraints Mask.Runtime context
      definition <- go context typex
      pure $ do
        definition <- definition
        pure
          Simple.TypeLambdaOver
            { parameters = TypePattern.typex' <$> parameters,
              constraints = Simple.Constraints.simplify constraints,
              result = definition
            }

solve ::
  Declaration (Unify.Solve s) (Unify.Logical s scope) locality Identity Group Check scope ->
  Unify.Solve s (Declaration Identity Void locality Identity Group Check scope)
solve Declaration {position, name, definition, typex = Identity (Solved typex)} = do
  definition <- Definition4.solve position definition
  typex <- Unify.solve position typex
  pure
    Declaration
      { position,
        name,
        definition,
        typex =
          Identity (Solved typex)
      }
