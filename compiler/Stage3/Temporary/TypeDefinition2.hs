module Stage3.Temporary.TypeDefinition2 where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import Stage1.Variable (QualifiedConstructor, QualifiedConstructorIdentifier)
import qualified Stage2.Index.Link.Type as Type
import Stage2.Scope (Environment (..), GroupType)
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.TypeDefinition2 as Solved
import Stage3.Check.Context (Context)
import Stage3.Temporary.TypeDefinition (TypeDefinition)
import qualified Stage3.Temporary.TypeDefinition as TypeDefinition
import qualified Stage3.Unify as Unify

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
  ST s (Solved.Element locality Check scope)
solveElement context Element {element, typex, position, name, constructorNames, link} = do
  element <- TypeDefinition.solve context element
  typex <- Unify.solve position typex
  pure Solved.Element {element, typex = Solved typex, position, name, constructorNames, link}
