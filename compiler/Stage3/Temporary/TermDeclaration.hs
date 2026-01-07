module Stage3.Temporary.TermDeclaration where

import Control.Monad.ST (ST)
import Error (unsupportedFeaturePatternLetBinds)
import Stage1.Position (Position)
import Stage1.Variable (Variable)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope (Local)
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.TypeAnnotation (TypeAnnotation)
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Simple.Scheme as Simple (Scheme)
import qualified Stage3.Simple.Scheme as Simple.Scheme
import Stage3.Simple.Type (lift)
import qualified Stage3.Simple.Type as Simple (simplify)
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import qualified Stage3.Tree.Scheme as Solved (Scheme (..))
import qualified Stage3.Tree.Scheme as Solved.Scheme
import qualified Stage3.Tree.TermDeclaration as Solved (TermDeclaration (..))
import qualified Stage3.Unify as Unify

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
  TypeAnnotation (Unify.Type s scope) scope ->
  Stage2.TermDeclaration scope ->
  ST s (TermDeclaration s scope)
check context annotation Stage2.Auto {Stage2.position, Stage2.definitionAuto, Stage2.name}
  | TypeAnnotation.Inferred typeAuto <- annotation = do
      definitionAuto <- Definition.check context typeAuto definitionAuto
      pure Auto {position, name, definitionAuto, typeAuto}
  | otherwise = error "bad type annotation"
check context annotation Stage2.Manual {Stage2.position, Stage2.definition, Stage2.name}
  | TypeAnnotation.Annotation
      { TypeAnnotation.annotation =
          annotation@Solved.Scheme
            { Solved.parameters,
              Solved.constraints,
              Solved.result
            }
      } <-
      annotation =
      do
        let simple = Simple.simplify result
        context <- Solved.Scheme.augment position parameters constraints context
        definition <- Definition.check context (lift simple) definition
        let typex = Simple.Scheme.simplify annotation
        pure Manual {position, name, definition, annotation, typex}
  | otherwise = error "bad type annotation"
check _ _ Stage2.Share {Stage2.position} = unsupportedFeaturePatternLetBinds position

solve :: TermDeclaration s scope -> ST s (Solved.TermDeclaration scope)
solve Manual {name, definition, annotation, typex} = do
  definition <- Definition.solve definition
  pure Solved.Manual {Solved.name, Solved.definition, Solved.annotation, Solved.typex}
solve Auto {position, name, definitionAuto, typeAuto} = do
  definitionAuto <- Definition.solve definitionAuto
  typeAuto <- Unify.solve position typeAuto
  pure Solved.Auto {Solved.name, Solved.definitionAuto, Solved.typeAuto}
