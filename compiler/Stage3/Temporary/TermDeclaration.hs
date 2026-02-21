module Stage3.Temporary.TermDeclaration where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Index.Term (Bound)
import Stage2.Shift (shift)
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.TypeAnnotation (Annotation (..), AnyAnnotation (..))
import qualified Stage3.Simple.Constraint as Simple.Constraint (lift)
import Stage3.Simple.Type (lift)
import qualified Stage3.Simple.Type as Simple.Type
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import Stage3.Temporary.Pattern (Pattern)
import qualified Stage3.Temporary.Pattern as Pattern
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Tree.TermDeclaration as Solved (Body (..), TermDeclaration (..))
import qualified Stage3.Tree.TypePattern as TypePattern
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple.Constraint (simplify)
import qualified Stage4.Tree.Type as Simple (simplify)

data TermDeclaration s scope
  = Auto
      { position :: !Position,
        name :: !Variable,
        body :: !(Unify.SchemeOver Body s scope)
      }
  | Manual
      { position :: !Position,
        name :: !Variable,
        body :: !(Unify.SchemeOver Body s scope),
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

data Body s scope
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

instance Unify.Zonk Body where
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

instance Unify.Generalizable Body where
  collect collector body = Unify.collect collector (typex body)

check ::
  Context s scope ->
  (Int -> ST s (Unify.Scheme s scope)) ->
  AnyAnnotation s scope ->
  Stage2.TermDeclaration scope ->
  ST s (TermDeclaration s scope)
check context _ annotation Stage2.Auto {position, definitionAuto, name} = case annotation of
  Global -> do
    body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
      typex <- Unify.fresh Unify.typex
      definition <- Definition.check context typex (shift definitionAuto)
      pure $ Body {definition, typex}
    pure Auto {position, name, body}
  Local typex -> do
    body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
      definition <- Definition.check context (shift typex) (shift definitionAuto)
      pure $ Body {definition, typex = shift typex}
    pure Auto {position, name, body}
  _ -> error "bad type annotation"
check context _ annotation Stage2.Manual {position, definition, name} = case annotation of
  AnyAnnotation
    Annotation
      { annotation =
          annotation@Solved.Scheme
            { parameters,
              constraints,
              result
            }
      } ->
      do
        let typex = lift $ Simple.simplify result
        context <- Solved.Scheme.augment position parameters constraints context
        definition <- Definition.check context typex definition
        let body =
              Unify.schemeOver
                (Simple.Type.lift . TypePattern.typex <$> parameters)
                (Simple.Constraint.lift . Simple.Constraint.simplify <$> constraints)
                Body {definition, typex}
        pure Manual {position, name, annotation, body}
  _ -> error "bad type annotation"
check context shared annotation Stage2.Share {name, position, shareIndex, patternx, bound} = do
  target <- shared shareIndex
  case annotation of
    Global -> do
      body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
        (full, instanciation) <- Unify.instanciate context position (shift target)
        patternx <- Pattern.check context full (shift patternx)
        let typex = patternx Pattern.! bound
        pure
          Shared
            { shareIndex,
              instanciation,
              patternx,
              bound,
              typex
            }
      pure Auto {position, name, body}
    Local typex' -> do
      body <- Unify.generalizeOver context $ Unify.Generalize $ \context -> do
        (full, instanciation) <- Unify.instanciate context position (shift target)
        patternx <- Pattern.check context full (shift patternx)
        let typex = patternx Pattern.! bound
        Unify.unify context position typex (shift typex')
        pure
          Shared
            { shareIndex,
              instanciation,
              patternx,
              bound,
              typex
            }
      pure Auto {position, name, body}
    AnyAnnotation
      Annotation
        { annotation =
            annotation@Solved.Scheme
              { parameters,
                constraints,
                result
              }
        } ->
        do
          let typex' = lift $ Simple.simplify result
          context <- Solved.Scheme.augment position parameters constraints context
          (full, instanciation) <- Unify.instanciate context position (shift target)
          patternx <- Pattern.check context full (shift patternx)
          let typex = patternx Pattern.! bound
          Unify.unify context position typex typex'
          let body =
                Unify.schemeOver
                  (Simple.Type.lift . TypePattern.typex <$> parameters)
                  (Simple.Constraint.lift . Simple.Constraint.simplify <$> constraints)
                  Shared
                    { shareIndex,
                      instanciation,
                      patternx,
                      bound,
                      typex
                    }
          pure Manual {position, name, annotation, body}

solve :: TermDeclaration s scope -> ST s (Solved.TermDeclaration scope)
solve = \case
  Manual {position, name, body, annotation} -> do
    body <- Unify.solveSchemeOver (Unify.Solve solveBody) position body
    pure Solved.Manual {name, body, annotation}
  Auto {position, name, body} -> do
    body <- Unify.solveSchemeOver (Unify.Solve solveBody) position body
    pure Solved.Auto {name, body}

solveBody :: Position -> Body s scope -> ST s (Solved.Body scope)
solveBody position = \case
  Body {definition, typex} -> do
    definition <- Definition.solve definition
    typex <- Unify.solve position typex
    pure Solved.Body {definition, typex}
  Shared {shareIndex, instanciation, patternx, bound, typex} -> do
    instanciation <- Unify.solveInstanciation position instanciation
    patternx <- Pattern.solve patternx
    typex <- Unify.solve position typex
    pure Solved.Shared {shareIndex, instanciation, patternx, bound, typex}
