module Builtin.Tuple where

import Core.Instanciate (Store (Empty))
import Core.Tree.Combinators.Delay (Delay (Here))
import Core.Tree.Constructor (ConstructorF (..))
import Core.Tree.Data (Data (Data), DefinitionF (..))
import qualified Core.Tree.Data as Data
import Core.Tree.Entry (EntryF (..))
import qualified Core.Tree.Type as Type
import Data.Foldable (Foldable (toList))
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Tree.Constructor (Syntax (..))
import Syntax.Lexer (constructorIdentifier)
import qualified Syntax.Tree.Brand as Brand
import qualified Syntax.Variable as Variable

definition :: Int -> Data scope
definition n =
  Data
    { parameters = Strict.Vector.replicate n Type.typex,
      definition =
        Definition
          { position = Empty,
            types = Here,
            constructors = Strict.Vector.fromList $ toList set,
            selectors = Strict.Vector.empty,
            brand = Brand.Boxed
          }
    }
  where
    set = map go [minBound .. maxBound]
    go Constructor.Tuple =
      Constructor
        { brand = Empty,
          entries = Strict.Vector.generate n $ \n ->
            Entry
              { position = Empty,
                entry = Type.Variable (Local.Local n),
                strict = Type.Constructor Type2.Lazy
              },
          syntax = Standard,
          name = Variable.ConstructorIdentifier $ constructorIdentifier $ pack $ "Tuple" ++ show n
        }
