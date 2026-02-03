module Stage3.Temporary.TermDeclaration where

import Control.Monad.ST (ST)
import Error (unsupportedFeaturePatternLetBinds)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Local)
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.TypeAnnotation (Annotation (..), AnyAnnotation (..))
import Stage3.Simple.Type (lift)
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Tree.TermDeclaration as Solved (TermDeclaration (..))
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Scheme as Simple (Scheme)
import qualified Stage4.Tree.Scheme as Simple.Scheme
import qualified Stage4.Tree.Type as Simple (simplify)

data TermDeclaration s scope
  = Auto
      { position :: !Position,
        name :: !Variable,
        definitionAuto :: !(Definition s scope),
        typeAuto :: !(Unify.Type s scope)
      }
  | Manual
      { position :: !Position,
        name :: !Variable,
        definition :: !(Definition s (Scope.Local ':+ scope)),
        annotation :: !(Solved.Scheme scope),
        typex :: !(Simple.Scheme scope)
      }

check ::
  Context s scope ->
  AnyAnnotation s scope ->
  Stage2.TermDeclaration scope ->
  ST s (TermDeclaration s scope)
check context annotation Stage2.Auto {position, definitionAuto, name} = case annotation of
  Global -> do
    typeAuto <- Unify.fresh Unify.typex
    definitionAuto <- Definition.check context typeAuto definitionAuto
    pure Auto {position, name, definitionAuto, typeAuto}
  Local typeAuto -> do
    definitionAuto <- Definition.check context typeAuto definitionAuto
    pure Auto {position, name, definitionAuto, typeAuto}
  _ -> error "bad type annotation"
check context annotation Stage2.Manual {position, definition, name} = case annotation of
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
        let simple = Simple.simplify result
        context <- Solved.Scheme.augment position parameters constraints context
        definition <- Definition.check context (lift simple) definition
        let typex = Simple.Scheme.simplify annotation
        pure Manual {position, name, definition, annotation, typex}
  _ -> error "bad type annotation"
check _ _ Stage2.Share {position} = unsupportedFeaturePatternLetBinds position

solve :: TermDeclaration s scope -> ST s (Solved.TermDeclaration scope)
solve Manual {name, definition, annotation, typex} = do
  definition <- Definition.solve definition
  pure Solved.Manual {name, definition, annotation, typex}
solve Auto {position, name, definitionAuto, typeAuto} = do
  definitionAuto <- Definition.solve definitionAuto
  typeAuto <- Unify.solve position typeAuto
  pure Solved.Auto {name, definitionAuto, typeAuto}
