module Stage3.Temporary.TermDeclaration where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Tree.Annotation (Annotated, Inferred)
import qualified Stage2.Tree.Annotation as Stage2 (Annotation (..))
import qualified Stage2.Tree.Definition2 as Stage2 (Definition2)
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..), TermDeclaration' (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.TypeAnnotation (Annotation (..), GlobalTypeAnnotation (..), LocalTypeAnnotation (..))
import qualified Stage3.Simple.Constraint as Simple.Constraint (lift)
import Stage3.Simple.Type (lift)
import qualified Stage3.Simple.Type as Simple.Type
import Stage3.Temporary.Definition2 (Definition2 (..))
import qualified Stage3.Temporary.Definition2 as Definition2
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Tree.TermDeclaration as Solved (TermDeclaration (..))
import qualified Stage3.Tree.TypePattern as TypePattern
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple.Constraint (simplify)
import qualified Stage4.Tree.Type as Simple (simplify)

data TermDeclaration s scope
  = Auto
      { position :: !Position,
        name :: !Variable,
        body :: !(Unify.SchemeOver Definition2 s scope)
      }
  | Manual
      { position :: !Position,
        name :: !Variable,
        body :: !(Unify.SchemeOver Definition2 s scope),
        annotation :: !(Solved.Scheme scope)
      }

instance Unify.Zonk TermDeclaration where
  zonk zonker = \case
    Auto {position, name, body} -> do
      body <- Unify.zonk zonker body
      pure Auto {position, name, body}
    Manual {position, name, body, annotation} -> do
      body <- Unify.zonk zonker body
      pure Manual {position, name, body, annotation}

checkLocal ::
  Context s scope ->
  (Int -> ST s (Unify.Scheme s scope)) ->
  LocalTypeAnnotation s scope ->
  Stage2.TermDeclaration scope ->
  ST s (TermDeclaration s scope)
checkLocal context shared annotation Stage2.TermDeclaration {position, name, declaration} =
  go declaration annotation
  where
    infix 0 `go`
    Stage2.Annotation {} Stage2.::: definition `go` LocalAnnotation annotation =
      check' context shared (Marked annotation) position name definition
    Stage2.NoAnnotation Stage2.::: definition `go` LocalInferred typex =
      check' context shared (Local typex) position name definition
    go _ _ = error "bad annotation"

checkGlobal ::
  Context s scope ->
  (Int -> ST s (Unify.Scheme s scope)) ->
  GlobalTypeAnnotation scope ->
  Stage2.TermDeclaration scope ->
  ST s (TermDeclaration s scope)
checkGlobal context shared annotation Stage2.TermDeclaration {position, name, declaration} =
  go declaration annotation
  where
    infix 0 `go`
    Stage2.Annotation {} Stage2.::: definition `go` GlobalAnnotation annotation =
      check' context shared (Marked annotation) position name definition
    Stage2.NoAnnotation Stage2.::: definition `go` GlobalInferred =
      check' context shared Global position name definition
    go _ _ = error "bad annotation"

data Which mark s scope where
  Global :: Which Inferred s scope
  Local :: !(Unify.Type s scope) -> Which Inferred s scope
  Marked :: !(Annotation scope) -> Which Annotated s scope

check' ::
  Context s scope ->
  (Int -> ST s (Unify.Scheme s scope)) ->
  Which mark s scope ->
  Position ->
  Variable ->
  Stage2.Definition2 mark scope ->
  ST s (TermDeclaration s scope)
check'
  context
  shared
  annotation
  position
  name
  definition = case annotation of
    Global -> do
      body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
        typex <- Unify.fresh Unify.typex
        Definition2.check context shared Definition2.Auto typex definition
      pure Auto {position, name, body}
    Local typex -> do
      body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
        Definition2.check context shared Definition2.Auto (shift typex) definition
      pure Auto {position, name, body}
    Marked Annotation {annotation} ->
      do
        body <- checkAnnotation context position annotation $ \context typex -> do
          Definition2.check context shared Definition2.Manual typex definition
        pure Manual {position, name, annotation, body}

checkAnnotation ::
  Context s scope ->
  Position ->
  Solved.Scheme scope ->
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
      context <- Solved.Scheme.augment position parameters constraints context
      definition <- go context typex
      pure $
        Unify.schemeOver
          (Simple.Type.lift . TypePattern.typex <$> parameters)
          (Simple.Constraint.lift . Simple.Constraint.simplify <$> constraints)
          definition

solve :: TermDeclaration s scope -> ST s (Solved.TermDeclaration scope)
solve = \case
  Manual {position, name, body, annotation} -> do
    body <- Unify.solveSchemeOver (Unify.Solve Definition2.solve) position body
    pure Solved.Manual {name, body, annotation}
  Auto {position, name, body} -> do
    body <- Unify.solveSchemeOver (Unify.Solve Definition2.solve) position body
    pure Solved.Auto {name, body}
