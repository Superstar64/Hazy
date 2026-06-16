module Semantic.Resolve.Temporary.Complete.Module where

import Control.Monad.Fix (mfix)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (Position, duplicateModuleEntries)
import qualified Semantic.Index.Term as Term
import qualified Semantic.Index.Term0 as Term0 (Index (..))
import qualified Semantic.Index.Type0 as Type0
import Semantic.Layout (Normal)
import Semantic.Resolve.Bindings (Bindings)
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Canonical (Canonical)
import Semantic.Resolve.Context (Context (..), (</>))
import qualified Semantic.Resolve.Context as Context
import Semantic.Resolve.Core (Core (Core))
import qualified Semantic.Resolve.Core as Core
import Semantic.Resolve.Import (pickImports, pickPrelude)
import qualified Semantic.Resolve.Import as Import
import Semantic.Resolve.Stability (Stability (..))
import Semantic.Resolve.Temporary.Complete.Declarations (Declarations)
import qualified Semantic.Resolve.Temporary.Complete.Declarations as Declarations
import Semantic.Scope (Global)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Module as Real
import Syntax.Extensions (Extensions (..))
import qualified Syntax.Tree.Declaration as Syntax (toImport)
import qualified Syntax.Tree.Declarations as Syntax (Declarations (..))
import qualified Syntax.Tree.Module as Syntax (Module (..), declarations, name)
import Syntax.Variable (FullQualifiers)
import qualified Syntax.Variable as Variable
import Verbose (Debug (creatingIndexes))

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }

resolve :: forall verbose. (Debug verbose) => Vector (Syntax.Module Position) -> verbose (Vector Module)
resolve modules = mfix $ \main ->
  let bindings :: Vector (Bindings () Global)
      bindings = Vector.imap bound main
        where
          bound index = Declarations.bindings (Term0.Global index) (Type0.Global index) . declarations

      canonical :: Canonical Global
      canonical = Import.pickModules (fmap context indexes)
        where
          context index =
            Import.Module
              { modulePosition,
                extensions,
                imports,
                exports,
                base = bindings Vector.! index
              }
            where
              Syntax.Module
                { modulePosition,
                  extensions,
                  exports,
                  declarations = Syntax.Declarations {declarations}
                } =
                  modules Vector.! index
              imports = mapMaybe Syntax.toImport (toList declarations)
      resolve :: Int -> Syntax.Module Position -> verbose Module
      resolve
        index
        Syntax.Module
          { extensions,
            modulePosition,
            name,
            declarations = ~Syntax.Declarations {declarations}
          } = make <$> resolved
          where
            imports = mapMaybe Syntax.toImport (toList declarations)
            make declarations = Module {name, declarations}
            resolved :: verbose (Declarations Global)
            resolved =
              creatingIndexes (Variable.print' name) $
                Declarations.resolve context extensions (Term.Global index) (toList declarations)
            context :: Context Global
            context
              | imports <- pickImports extensions (toList imports) canonical =
                  shadow </> (prelude <> imports) </> (Context.empty extensions) {Context.canonical}
            Extensions {implicitPrelude} = extensions
            prelude
              | implicitPrelude = pickPrelude modulePosition (toList imports) canonical
              | otherwise = Core {globals = Map.empty, locals = mempty}
            binding = Bindings.updateStability (Stable [modulePosition]) $ bindings Vector.! index
            shadow =
              Core
                { globals = Map.singleton name binding,
                  locals = binding
                }
   in sequence $ Vector.imap resolve modules
  where
    names = Vector.map Syntax.name modules
    indexes :: Map FullQualifiers Int
    indexes = Map.fromListWith duplicate $ zip (toList names) [0 ..]
      where
        duplicate left right
          | Syntax.Module {modulePosition = left} <- modules Vector.! left,
            Syntax.Module {modulePosition = right} <- modules Vector.! right =
              duplicateModuleEntries [left, right]

shrink :: Module -> Real.Module Normal Resolve
shrink Module {name, declarations} =
  Real.Module
    { name,
      declarations = Declarations.shrink declarations
    }
