module Stage3.Tree.CallHead where

import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term as Term
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import Stage3.Tree.MethodInfo (MethodInfo)
import Stage3.Tree.SelectorInfo (SelectorInfo)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation)

data CallHead scope
  = Variable
      { variable :: !(Term.Index scope),
        instanciation :: !(Simple.Instanciation scope)
      }
  | Constructor
      { constructor :: !(Constructor.Index scope),
        constructorInfo :: !ConstructorInfo
      }
  | Selector
      { selector :: !(Selector.Index scope),
        selectorInfo :: !SelectorInfo
      }
  | Method
      { method :: !(Method.Index scope),
        evidence :: !(Simple.Evidence scope),
        instanciation :: !(Simple.Instanciation scope),
        methodInfo :: !MethodInfo
      }
  deriving (Show)
