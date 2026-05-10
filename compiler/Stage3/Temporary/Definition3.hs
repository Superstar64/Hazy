module Stage3.Temporary.Definition3 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Tree.Definition2 (Annotated, Inferred)
import Stage2.Tree.Definition3 (Info)
import qualified Stage2.Tree.Definition3 as Stage2
import Stage3.Check.Context (Context)
import qualified Stage3.Check.Mask as Mask
import qualified Stage3.Simple.Constraint as Simple.Constraint (lift)
import Stage3.Simple.Type (lift)
import Stage3.Temporary.Definition2 (Definition2)
import qualified Stage3.Temporary.Definition2 as Definition2
import qualified Stage3.Tree.Definition3 as Solved (Definition3 (..))
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Tree.TypePattern as TypePattern
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple.Constraint (simplify)
import qualified Stage4.Tree.Type as Simple (simplify)

data Definition3 mark s scope where
  (::@) :: !(Info source) -> !(Unify.SchemeOver (Definition2 source mark) s scope) -> Definition3 mark s scope

infixr 5 ::@

instance Unify.Zonk (Definition3 mark) where
  zonk zonker (info ::@ definition) = do
    definition <- Unify.zonk zonker definition
    pure $ info ::@ definition

data Which mark s scope where
  Global :: Which Inferred s scope
  Local :: !(Unify.Type s scope) -> Which Inferred s scope
  Marked :: !(Solved.Scheme scope) -> Which Annotated s scope

check ::
  Context s scope ->
  Which mark s scope ->
  Position ->
  Stage2.Definition3 mark scope ->
  ST s (Definition3 mark s scope)
check context annotation position (info Stage2.::@ definition) =
  (info ::@) <$> case annotation of
    Global -> Unify.generalizeOver context $ Unify.Generalize $ \context -> do
      typex <- Unify.fresh Unify.typex
      Definition2.check context Definition2.Auto typex definition
    Local typex -> Unify.generalizeOver context $ Unify.Generalize $ \context -> do
      Definition2.check context Definition2.Auto (shift typex) definition
    Marked annotation ->
      checkAnnotation context position annotation $ \context typex -> do
        Definition2.check context Definition2.Manual typex definition

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
      context <- Solved.Scheme.augment position parameters constraints Mask.Runtime context
      definition <- go context typex
      pure $
        Unify.schemeOver
          (lift . TypePattern.typex <$> parameters)
          (Simple.Constraint.lift . Simple.Constraint.simplify <$> constraints)
          definition

solve :: Position -> Definition3 mark s scope -> ST s (Solved.Definition3 mark scope)
solve position (info ::@ definition) = do
  definition <- Unify.solveSchemeOver (Unify.Solve Definition2.solve) position definition
  pure $ info Solved.::@ definition
