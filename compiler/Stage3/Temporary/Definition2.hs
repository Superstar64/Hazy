module Stage3.Temporary.Definition2 where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Index.Term (Bound)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import Stage2.Tree.Annotation (Annotated, Inferred)
import qualified Stage2.Tree.Definition2 as Stage2
import Stage3.Check.Context (Context)
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Tree.Definition2 as Solved (Definition2 (..))
import qualified Stage3.Unify as Unify

data Definition2 s scope
  = Body
      { definition :: !(Definition s scope),
        typex :: !(Unify.Type s scope)
      }
  | Shared
      { shareIndex :: !Int,
        instanciation :: !(Unify.Instanciation s scope),
        patternx :: !(Pattern s scope),
        bound :: !Bound,
        typex :: !(Unify.Type s scope)
      }

instance Unify.Zonk Definition2 where
  zonk zonker = \case
    Body {definition, typex} -> do
      definition <- Unify.zonk zonker definition
      typex <- Unify.zonk zonker typex
      pure Body {definition, typex}
    Shared {shareIndex, instanciation, patternx, bound, typex} -> do
      instanciation <- Unify.zonk zonker instanciation
      patternx <- Unify.zonk zonker patternx
      typex <- Unify.zonk zonker typex
      pure Shared {shareIndex, instanciation, patternx, bound, typex}

instance Unify.Generalizable Definition2 where
  collect collector body = Unify.collect collector (typex body)

data Which mark scope where
  Manual :: Which Annotated (Local ':+ scope)
  Auto :: Which Inferred scope

check ::
  Context s (scope ':+ scopes) ->
  (Int -> ST s (Unify.Scheme s scopes)) ->
  Which mark (scope ':+ scopes) ->
  Unify.Type s (scope ':+ scopes) ->
  Stage2.Definition2 mark scopes ->
  ST s (Definition2 s (scope ':+ scopes))
check context _ Auto typex (Stage2.Auto definition) = do
  definition <- Definition.check context typex (shift definition)
  pure $ Body {definition, typex}
check context _ Manual typex (Stage2.Manual definition) = do
  definition <- Definition.check context typex definition
  pure Body {definition, typex}
check context shared _ typex declaration = case declaration of
  Stage2.Share Stage2.Choice {position, shareIndex, patternx, bound} -> do
    target <- shared shareIndex
    (full, instanciation) <- Unify.instanciate context position (shift target)
    patternx <- Pattern.check context full (shift patternx)
    let typex' = patternx Pattern.! bound
    Unify.unify context position typex typex'
    pure Shared {shareIndex, instanciation, patternx, bound, typex}

solve :: Position -> Definition2 s scope -> ST s (Solved.Definition2 scope)
solve position = \case
  Body {definition, typex} -> do
    definition <- Definition.solve definition
    typex <- Unify.solve position typex
    pure Solved.Body {definition, typex}
  Shared {shareIndex, instanciation, patternx, bound, typex} -> do
    instanciation <- Unify.solveInstanciation position instanciation
    patternx <- Pattern.solve patternx
    typex <- Unify.solve position typex
    pure Solved.Shared {shareIndex, instanciation, patternx, bound, typex}
