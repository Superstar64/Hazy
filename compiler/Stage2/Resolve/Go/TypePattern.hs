module Stage2.Resolve.Go.TypePattern where

import qualified Stage1.Tree.TypePattern as Stage1
import Stage2.Stage (Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.TypePattern (TypePattern (..))

resolve :: Stage1.TypePattern position -> TypePattern position Resolve scope
resolve Stage1.TypePattern {position, name} =
  TypePattern
    { position,
      name,
      typex = Inferred
    }
