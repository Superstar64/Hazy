module Stage2.Temporary.Complete.Module where

import Control.Monad.Fix (mfix)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (Position, duplicateModuleEntries)
import Stage1.Extensions (Extensions (..))
import qualified Stage1.Tree.Declaration as Stage1 (toImport)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations (..))
import qualified Stage1.Tree.Module as Stage1 (Module (..), declarations, name)
import Stage1.Variable (FullQualifiers)
import qualified Stage1.Variable as Variable
import qualified Stage2.Index.Term0 as Term0 (Index (..))
import qualified Stage2.Index.Type as Type
import Stage2.Resolve.Bindings (Bindings)
import qualified Stage2.Resolve.Bindings as Bindings
import Stage2.Resolve.Canonical (Canonical)
import Stage2.Resolve.Context (Context (..), (</>))
import qualified Stage2.Resolve.Context as Context
import Stage2.Resolve.Core (Core (Core))
import qualified Stage2.Resolve.Core as Core
import Stage2.Resolve.Import (StableImports (StableImports), pickImports, pickPrelude)
import qualified Stage2.Resolve.Import as Import
import Stage2.Resolve.Stability (Stability (..))
import Stage2.Scope (Global)
import Stage2.Temporary.Complete.Declarations (Declarations)
import qualified Stage2.Temporary.Complete.Declarations as Declarations
import qualified Stage2.Tree.Module as Real
import Verbose (Debug (creatingIndexes))

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }

resolve :: forall verbose. (Debug verbose) => Vector (Stage1.Module Position) -> verbose (Vector Module)
resolve modules = mfix $ \main ->
  let bindings :: Vector (Bindings () Global)
      bindings = Vector.imap bound main
        where
          bound index = Declarations.bindings (Term0.Global index) (Type.Global index) . declarations

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
              Stage1.Module
                { modulePosition,
                  extensions,
                  exports,
                  declarations = Stage1.Declarations {declarations}
                } =
                  modules Vector.! index
              imports = mapMaybe Stage1.toImport (toList declarations)
      resolve :: Int -> Stage1.Module Position -> verbose Module
      resolve
        index
        Stage1.Module
          { extensions,
            modulePosition,
            name,
            declarations = ~Stage1.Declarations {declarations}
          } = make <$> resolved
          where
            imports = mapMaybe Stage1.toImport (toList declarations)
            make declarations = Module {name, declarations}
            resolved :: verbose (Declarations Global)
            resolved =
              creatingIndexes (Variable.print' name) $
                Declarations.resolve context extensions (toList declarations)
            context :: Context Global
            context
              | imports <- pickImports (StableImports stableImports) (toList imports) canonical =
                  shadow </> (prelude <> imports) </> (Context.empty extensions) {Context.canonical}
            Extensions {implicitPrelude, stableImports} = extensions
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
    names = Vector.map Stage1.name modules
    indexes :: Map FullQualifiers Int
    indexes = Map.fromListWith duplicate $ zip (toList names) [0 ..]
      where
        duplicate left right
          | Stage1.Module {modulePosition = left} <- modules Vector.! left,
            Stage1.Module {modulePosition = right} <- modules Vector.! right =
              duplicateModuleEntries [left, right]

shrink :: Module -> Real.Module
shrink Module {name, declarations} =
  Real.Module
    { name,
      declarations = Declarations.shrink declarations
    }
