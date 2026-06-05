module Semantic.Resolve.Detail.Binding.Constructor where

import Data.Map (Map)
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Data.Void (absurd)
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Resolve.Functor.Same (Same (..))
import Syntax.Tree.Fixity (Fixity)
import Syntax.Variable (Variable)

data Binding scope = Binding
  { index :: !(Constructor.Index scope),
    fixity :: !Fixity,
    fields :: !(Map Variable Int),
    selections :: !(Strict.Vector (Strict.Maybe Int)),
    unordered :: !Bool,
    fielded :: !Bool,
    single :: !Bool
  }

instance Same (Binding scope) where
  same abort left right
    | left == right = left
    | otherwise = absurd abort

instance Eq (Binding scope) where
  left == right = index left == index right
