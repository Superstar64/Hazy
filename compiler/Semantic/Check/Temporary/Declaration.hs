module Semantic.Check.Temporary.Declaration where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraint as Simple.Constraint (simplify)
import qualified Core.Tree.Type as Simple (simplify)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..), groupTermBindings)
import qualified Semantic.Check.Go.Scheme as Solved.Scheme
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Constraint as Simple.Constraint (lift)
import qualified Semantic.Check.Simple.Scheme as Simple.Scheme
import Semantic.Check.Simple.Type (lift)
import qualified Semantic.Check.Temporary.Definition3 as Definition3
import Semantic.Check.Temporary.Definition4 (Definition4 (..), Element (Element), Types (..), solveElement)
import qualified Semantic.Check.Temporary.Definition4 as Definition4
import Semantic.Check.TypeAnnotation (Annotation (..), TypeAnnotation (..))
import qualified Semantic.Index.Link.Term as Term
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Category (Shift), shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Declaration (Key)
import qualified Semantic.Tree.Declaration as Semantic (Declaration (..))
import qualified Semantic.Tree.Declaration as Solved (Declaration (..))
import qualified Semantic.Tree.Definition4 as Semantic
  ( Annotation (..),
    Definition4 (..),
    Element (..),
    Set (..),
  )
import qualified Semantic.Tree.Definition4 as Solved (Set (..))
import Semantic.Tree.Scheme as Solved (Scheme (..))
import qualified Semantic.Tree.TypePattern as TypePattern
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Declaration locality s scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    definition :: !(Definition4 locality s scope),
    typex :: !(Unify.Scheme s scope)
  }

typex' = typex

check ::
  Context s scope ->
  (Term.Link locality -> Int -> ST s (Unify.Scheme s scope)) ->
  TypeAnnotation scope ->
  Semantic.Declaration locality Group Resolve scope ->
  ST s (Declaration locality s scope)
check context linked annotation Semantic.Declaration {position, name, definition} = case definition of
  Semantic.Annotated {} Semantic.::: Implicit.Resolve definition
    | Annotated Annotation {annotation, annotation'} <- annotation -> do
        definition <- checkAnnotation context position annotation $ \context typex ->
          Definition3.checkManual context typex definition
        pure
          Declaration
            { position,
              name,
              definition = Semantic.Annotated annotation ::: definition,
              typex = Simple.Scheme.lift annotation'
            }
    | otherwise -> error "bad type annotation"
  _ Semantic.:::: Implicit.Resolve (Semantic.Set set) -> do
    types Unify.::: set <- Unify.generalizeBody position context $ Unify.Generalize $ \context -> do
      fresh <- Vector.replicateM (length set) $ Unify.fresh Unify.typex
      set <- flip Strict.Vector.imapM set $ \index Semantic.Element {element, link} -> do
        let element' = Shift.map (Shift.Over Shift) element
            typex = fresh Vector.! index
        element <- Definition3.checkAuto (groupTermBindings fresh context) (shift typex) element'
        pure Element {element, link}
      let types = Types (Strict.Vector.fromLazy fresh)
          solved = do
            set <- traverse (solveElement position) set
            pure $ Solved.Set set
      pure $ types Unify.::: solved
    let initial = Unify.MapScheme $ \(Types types) -> Strict.Vector.head types
    pure
      Declaration
        { position,
          name,
          definition = types Definition4.:::: set,
          typex = Unify.Scheme $ Unify.mapScheme initial types
        }
  Semantic.Link link id -> do
    typex <- linked link id
    pure
      Declaration
        { position,
          name,
          definition = Link link id,
          typex
        }

checkAnnotation ::
  Context s scope ->
  Position ->
  Solved.Scheme Position Check scope ->
  ( Context s (Local ':+ scope) ->
    Unify.Type s (Local ':+ scope) ->
    ST s (typex s (Local ':+ scope))
  ) ->
  ST s (Unify.SchemeOver typex s scope)
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
      let typex = lift $ Simple.simplify result
      context <- Solved.Scheme.augment position parameters constraints Mask.Runtime context
      definition <- go context typex
      pure $
        Unify.schemeOver
          (lift . TypePattern.typex' <$> parameters)
          (Simple.Constraint.lift . Simple.Constraint.simplify <$> constraints)
          definition

solve :: Declaration locality s scope -> Unify.Solve s (Solved.Declaration locality Group Check scope)
solve Declaration {position, name, definition, typex} = do
  definition <- Definition4.solve position definition
  typex <- Unify.solveScheme position typex
  pure Solved.Declaration {position, name, definition, typex = Solved typex}
