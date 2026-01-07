module Stage2.Resolve.Detail.Binding.Constructor where

import Data.Map (Map)
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Data.Void (absurd)
import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import qualified Stage2.Index.Constructor as Constructor
import Stage2.Resolve.Functor.Same (Same (..))

data Binding scope = Binding
  { index :: !(Constructor.Index scope),
    fixity :: !Fixity,
    fields :: !(Map Variable Int),
    selections :: !(Strict.Vector (Strict.Maybe Int)),
    unordered :: !Bool,
    fielded :: !Bool
  }

instance Same (Binding scope) where
  same abort left right
    | left == right = left
    | otherwise = absurd abort

instance Eq (Binding scope) where
  left == right = index left == index right
