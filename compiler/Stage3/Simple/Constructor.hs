module Stage3.Simple.Constructor where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Constructor (Constructor (..))

instanciate :: Strict.Vector (Unify.Type s scope) -> Constructor (Local ':+ scope) -> ConstructorInstance s scope
instanciate fresh Constructor {entries} =
  ConstructorInstance
    { entries = Type.instanciate fresh <$> entries
    }
