module Stage3.Simple.Class (Class (..), kind, instanciate) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage3.Check.ClassInstance (ClassInstance (ClassInstance))
import qualified Stage3.Check.ClassInstance as ClassInstance
import Stage3.Check.Context (Context)
import qualified Stage3.Simple.Scheme as Simple.Scheme
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} Stage4.Tree.Class (Class (..), kind)

instanciate :: Context s scope -> Position -> Type2.Index scope -> Class scope -> ST s (ClassInstance s scope)
instanciate context position index Class {parameter, methods} = do
  typex <- Unify.fresh (Type.lift parameter)
  evidence <- Unify.constrain context position index typex
  let types = Strict.Vector.singleton typex
  methods <- pure $ Simple.Scheme.instanciate' types <$> methods
  pure
    ClassInstance
      { typex,
        evidence,
        methods
      }
