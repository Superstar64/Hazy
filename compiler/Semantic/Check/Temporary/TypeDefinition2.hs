module Semantic.Check.Temporary.TypeDefinition2 where

import qualified Data.Vector.Strict as Strict
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.TypeDefinition (TypeDefinition)
import qualified Semantic.Check.Temporary.TypeDefinition as TypeDefinition
import qualified Semantic.Index.Link.Type as Type
import Semantic.Scope (Environment (..), GroupType)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.TypeDefinition2 as Solved
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Variable (QualifiedConstructor, QualifiedConstructorIdentifier)

data Element locality s scope = Element
  { element :: !(TypeDefinition s (GroupType ':+ scope)),
    typex :: !(Unify.Type s scope),
    position :: !Position,
    name :: !QualifiedConstructorIdentifier,
    constructorNames :: !(Strict.Vector QualifiedConstructor),
    link :: !(Type.Link locality)
  }

solveElement ::
  Context s (GroupType ':+ scope) ->
  Element locality s scope ->
  Unify.Solve s (Solved.Element locality Check scope)
solveElement context Element {element, typex, position, name, constructorNames, link} = do
  element <- TypeDefinition.solve context element
  typex <- Unify.solve position typex
  pure Solved.Element {element, typex = Solved typex, position, name, constructorNames, link}
