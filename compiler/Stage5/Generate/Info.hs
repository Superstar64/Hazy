module Stage5.Generate.Info where

import qualified Stage2.Index.Type2 as Type2
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type

strict :: Type scope -> Bool
strict = \case
  Type.Constructor Type2.Lazy -> False
  Type.Constructor Type2.Strict -> True
  _ -> error "bad type info"
