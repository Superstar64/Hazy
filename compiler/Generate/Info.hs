module Generate.Info where

import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Semantic.Index.Type2 as Type2

strict :: Type scope -> Bool
strict = \case
  Type.Constructor Type2.Lazy -> False
  Type.Constructor Type2.Strict -> True
  _ -> error "bad type info"
