module Semantic.Check.Simple.Class where

import Control.Monad.ST (ST)
import Core.Substitute (logicalType, substituteType)
import Core.Tree.Class (Class (..), DefinitionF (..))
import qualified Core.Tree.Class as Class
import Core.Tree.Combinators.Delay (Delay (..))
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.ClassInstance (ClassInstance)
import Semantic.Check.Context (Context)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

instanciate ::
  Context s scope ->
  Position ->
  Type2.Index scope ->
  Class scope ->
  ST s (ClassInstance s scope)
instanciate
  context
  position
  index
  Class {parameter, constraints, definition = Definition {methods}} = do
    typex <- Unify.fresh (logicalType parameter)
    evidence <- Unify.constrain context position index typex
    let types = Strict.Vector.singleton typex
    methods <- pure $ substituteType (Strict.Vector.toLazy types) <$> methods
    pure
      Class.Definition
        { typex = Delay typex,
          evidence = Delay evidence,
          methods,
          constraintCount = length constraints
        }
