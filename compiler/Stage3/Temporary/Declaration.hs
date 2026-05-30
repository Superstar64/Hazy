module Stage3.Temporary.Declaration where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Normal)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.Declaration (Key)
import qualified Stage2.Tree.Declaration as Solved (Declaration (..))
import qualified Stage2.Tree.Declaration as Stage2 (Declaration (..))
import Stage2.Tree.Definition4 (Annotation (..))
import qualified Stage2.Tree.Definition4 as Stage2 (Annotation (..), Definition4 (..))
import qualified Stage2.Tree.TypePattern as TypePattern
import Stage3.Check.Context (Context (..))
import qualified Stage3.Check.Mask as Mask
import Stage3.Check.TypeAnnotation (Annotation (..), GlobalTypeAnnotation (..), LocalTypeAnnotation (..))
import qualified Stage3.Simple.Constraint as Simple.Constraint (lift)
import Stage3.Simple.Type (lift)
import qualified Stage3.Temporary.Definition3 as Definition3
import Stage3.Temporary.Definition4 (Definition4 (..))
import qualified Stage3.Temporary.Definition4 as Definition4
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple.Constraint (simplify)
import qualified Stage4.Tree.Type as Simple (simplify)

data Declaration s scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    definition :: !(Definition4 s scope),
    typex :: !(Unify.Scheme s scope)
  }

instance Unify.Zonk Declaration where
  zonk zonker = \case
    Declaration {position, name, definition, typex} -> do
      definition <- Unify.zonk zonker definition
      typex <- Unify.zonk zonker typex
      pure
        Declaration
          { position,
            name,
            definition,
            typex
          }

checkLocal ::
  Context s scope ->
  LocalTypeAnnotation s scope ->
  Stage2.Declaration locality Normal Resolve scope ->
  ST s (Declaration s scope)
checkLocal context annotation Stage2.Declaration {position, name, definition} = do
  definition@(_ ::: inner) <- definition `go` annotation
  pure
    Declaration
      { position,
        name,
        definition,
        typex = Unify.Scheme $ Unify.mapScheme (Unify.MapScheme Definition3.typex) inner
      }
  where
    infix 0 `go`
    Stage2.Annotated {} Stage2.::: Implicit.Resolve definition `go` LocalAnnotation Annotation {annotation} = do
      definition <- checkAnnotation context position annotation $ \context typex -> do
        Definition3.checkManual context typex definition
      pure $ Annotated annotation ::: definition
    Stage2.Inferred {} Stage2.::: Implicit.Resolve definition `go` LocalInferred typex = do
      definition <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
        Definition3.checkAuto context (shift typex) (shift definition)
      pure $ Inferred ::: definition
    go _ _ = error "bad annotation"

checkGlobal ::
  Context s scope ->
  GlobalTypeAnnotation scope ->
  Stage2.Declaration locality Normal Resolve scope ->
  ST s (Declaration s scope)
checkGlobal context annotation Stage2.Declaration {position, name, definition} = do
  definition@(_ ::: inner) <- definition `go` annotation
  pure
    Declaration
      { position,
        name,
        definition,
        typex = Unify.Scheme $ Unify.mapScheme (Unify.MapScheme Definition3.typex) inner
      }
  where
    infix 0 `go`
    Stage2.Annotated {} Stage2.::: Implicit.Resolve definition `go` GlobalAnnotation Annotation {annotation} = do
      definition <- checkAnnotation context position annotation $ \context typex -> do
        Definition3.checkManual context typex definition
      pure $ Annotated annotation ::: definition
    Stage2.Inferred {} Stage2.::: Implicit.Resolve definition `go` GlobalInferred = do
      definition <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
        typex <- Unify.fresh Unify.typex
        Definition3.checkAuto context typex (shift definition)
      pure $ Inferred ::: definition
    go _ _ = error "bad annotation"

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

solve :: Declaration s scope -> ST s (Solved.Declaration locality Normal Check scope)
solve Declaration {position, name, definition, typex} = do
  definition <- Definition4.solve position definition
  typex <- Unify.solveScheme position typex
  pure Solved.Declaration {position, name, definition, typex = Solved typex}
