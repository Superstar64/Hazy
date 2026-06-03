module Stage3.Temporary.Declaration where

import Control.Monad.ST (ST)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Link.Term as Term
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Category (Shift), shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.Declaration (Key)
import qualified Stage2.Tree.Declaration as Solved (Declaration (..))
import qualified Stage2.Tree.Declaration as Stage2 (Declaration (..))
import qualified Stage2.Tree.Definition4 as Solved (Set (..))
import qualified Stage2.Tree.Definition4 as Stage2
  ( Annotation (..),
    Definition4 (..),
    Element (..),
    Set (..),
  )
import Stage2.Tree.Scheme as Solved (Scheme (..))
import qualified Stage2.Tree.TypePattern as TypePattern
import Stage3.Check.Context (Context (..), groupTermBindings)
import qualified Stage3.Check.Mask as Mask
import Stage3.Check.TypeAnnotation (Annotation (..), TypeAnnotation (..))
import qualified Stage3.Simple.Constraint as Simple.Constraint (lift)
import qualified Stage3.Simple.Scheme as Simple.Scheme
import Stage3.Simple.Type (lift)
import qualified Stage3.Temporary.Definition3 as Definition3
import Stage3.Temporary.Definition4 (Definition4 (..), Element (Element), Types (..), solveElement)
import qualified Stage3.Temporary.Definition4 as Definition4
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple.Constraint (simplify)
import qualified Stage4.Tree.Type as Simple (simplify)

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
  Stage2.Declaration locality Group Resolve scope ->
  ST s (Declaration locality s scope)
check context linked annotation Stage2.Declaration {position, name, definition} = case definition of
  Stage2.Annotated {} Stage2.::: Implicit.Resolve definition
    | Annotated Annotation {annotation, annotation'} <- annotation -> do
        definition <- checkAnnotation context position annotation $ \context typex ->
          Definition3.checkManual context typex definition
        pure
          Declaration
            { position,
              name,
              definition = Stage2.Annotated annotation ::: definition,
              typex = Simple.Scheme.lift annotation'
            }
    | otherwise -> error "bad type annotation"
  _ Stage2.:::: Implicit.Resolve (Stage2.Set set) -> do
    types Unify.::: set <- Unify.generalizeBody position context $ Unify.Generalize $ \context -> do
      fresh <- Vector.replicateM (length set) $ Unify.fresh Unify.typex
      set <- flip Strict.Vector.imapM set $ \index Stage2.Element {element, link} -> do
        let element' = Shift.map (Shift.Over Shift) element
            typex = fresh Vector.! index
        element <- Definition3.checkAuto (groupTermBindings fresh context) (shift typex) element'
        pure Element {element, link}
      let types = Types (Strict.Vector.fromLazy fresh)
          solved = Unify.Delay $ do
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
  Stage2.Link link id -> do
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

solve :: Declaration locality s scope -> ST s (Solved.Declaration locality Group Check scope)
solve Declaration {position, name, definition, typex} = do
  definition <- Definition4.solve position definition
  typex <- Unify.solveScheme position typex
  pure Solved.Declaration {position, name, definition, typex = Solved typex}
