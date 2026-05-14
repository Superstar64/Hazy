module Stage3.Tree.TypeDeclarationExtra where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment (..), Local)
import Stage3.Tree.Definition (Definition)

data TypeDeclarationExtra scope
  = ADT
  | Class
      { defaults :: !(Strict.Vector (Strict.Maybe (Definition (Local ':+ Local ':+ scope))))
      }
  | Synonym
  | GADT
  deriving (Show)
