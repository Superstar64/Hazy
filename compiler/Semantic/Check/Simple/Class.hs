module Semantic.Check.Simple.Class where

import Control.Monad.ST (ST)
import {-# SOURCE #-} Core.Tree.Class (Class (..))
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.ClassInstance (ClassInstance (ClassInstance))
import qualified Semantic.Check.ClassInstance as ClassInstance
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Simple.Scheme as Simple.Scheme
import qualified Semantic.Check.Simple.Type as Type
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

instanciate :: Context s scope -> Position -> Type2.Index scope -> Class scope -> ST s (ClassInstance s scope)
instanciate context position index Class {parameter, constraints, methods} = do
  typex <- Unify.fresh (Type.lift parameter)
  evidence <- Unify.constrain context position index typex
  let types = Strict.Vector.singleton typex
  methods <- pure $ Simple.Scheme.instanciate' types <$> methods
  pure
    ClassInstance
      { typex,
        evidence,
        methods,
        constraintCount = length constraints
      }
