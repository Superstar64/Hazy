module Generate.Go.Module where

import Control.Monad.ST (ST, runST)
import Core.Tree.Declaration (Declaration (..))
import qualified Core.Tree.Declaration as Term
import Core.Tree.Declarations (Declarations (..))
import qualified Core.Tree.Module as Core (Module (..))
import qualified Core.Tree.TypeDeclaration as Type (TypeDeclaration (..))
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.STRef (readSTRef)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Generate.Context as Context
import Generate.Global (Global (Global))
import qualified Generate.Global as Global
import Generate.GlobalType (GlobalType (GlobalType))
import qualified Generate.GlobalType as GlobalType
import qualified Generate.Go.Expression as Expression
import qualified Generate.Go.Instance as Instance
import qualified Generate.Mangle as Mangle
import Generate.Precontext (Precontext (Precontext))
import qualified Generate.Precontext as Precontext
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import qualified Semantic.Index.Type as Type (Index (..))
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import Syntax.Lexer (FullQualifiers)
import Syntax.Variable (FullyQualifiedConstructorIdentifier ((:.=.)))

data Module = Module
  { name :: !FullQualifiers,
    statements :: [Javascript.Statement 'False]
  }

generate :: Vector Core.Module -> Vector Module
generate modules = Vector.imap go modules
  where
    pre = precontext modules
    go index Core.Module {name, declarations} =
      let statements = runST (generate' pre name index declarations)
       in Module {name, statements}

precontext :: Vector Core.Module -> Precontext
precontext modules =
  Precontext
    { terms = names <$> modules,
      types = instanceNames <$> modules
    }
  where
    names Core.Module {name = path, declarations = Declarations {terms}} = generate <$> terms
      where
        generate Term.Declaration {name} =
          Global
            { path,
              name = Mangle.mangle name
            }
    instanceNames
      Core.Module
        { name = path,
          declarations = Declarations {types, classInstances, dataInstances}
        } = Vector.zipWith3 generate types classInstances dataInstances
        where
          global name = Global {path, name}
          generate Type.TypeDeclaration {name} classInstances dataInstances =
            GlobalType
              { classInstances =
                  let go key _ = global $ single Mangle.Class key
                   in Map.mapWithKey go classInstances,
                dataInstances =
                  let go key _ = global $ single Mangle.Data key
                   in Map.mapWithKey go dataInstances
              }
            where
              single :: Mangle.Brand -> Type2.Index Scope.Global -> Text
              single brand = Mangle.mangleInstance target brand name
              target :: Type.Index Scope.Global -> FullyQualifiedConstructorIdentifier
              target (Type.Global global local) = targetPath :.=. targetName
                where
                  Core.Module
                    { name = targetPath,
                      declarations = Declarations {types}
                    } = modules Vector.! global
                  Type.TypeDeclaration {name = targetName} = types Vector.! local

generate' :: Precontext -> FullQualifiers -> Int -> Declarations Scope.Global -> ST s [Javascript.Statement 'False]
generate'
  precontext@Precontext
    { terms = termNames,
      types = typeNames
    }
  moduleName
  moduleIndex
  Declarations
    { terms,
      classInstances,
      dataInstances
    } = do
    context <- Context.start precontext
    statements <- for (zip [0 ..] (toList terms)) $ \(termIndex, Declaration {definition}) ->
      do
        thunk <- Expression.declaration context definition
        source <- Context.fresh context
        let Global {name} = termNames Vector.! moduleIndex Vector.! termIndex
        pure
          [ Javascript.Const source thunk,
            Javascript.Export source name
          ]
    classStatements <- for
      (zip [0 ..] $ toList classInstances)
      $ \(typeIndex, classInstances) ->
        for (Map.toList classInstances) $ \(targetIndex, instancex) -> do
          instancex <- Instance.generate context instancex
          source <- Context.fresh context
          let Global {name} =
                typeNames Vector.! moduleIndex Vector.! typeIndex `GlobalType.indexClass` targetIndex
          pure
            [ Javascript.Const source instancex,
              Javascript.Export source name
            ]
    dataStatements <- for
      (zip [0 ..] $ toList dataInstances)
      $ \(typeIndex, dataInstances) ->
        for (Map.toList dataInstances) $ \(targetIndex, instancex) -> do
          instancex <- Instance.generate context instancex
          source <- Context.fresh context
          let Global {name} =
                typeNames Vector.! moduleIndex Vector.! typeIndex `GlobalType.indexData` targetIndex
          pure
            [ Javascript.Const source instancex,
              Javascript.Export source name
            ]
    wanted <- readSTRef (Context.used context)
    imports <- for (Map.toList wanted) $ \(Global {path, name}, temporary) -> do
      pure $ Javascript.Import name temporary (Mangle.depth moduleName <> Mangle.pathJS path <> Mangle.mjs)
    let builtin = toList $ importBuiltin <$> Mangle.canonical <*> Mangle.builtin
        importBuiltin canonical target =
          Javascript.Import
            canonical
            target
            (Mangle.depth moduleName <> Mangle.runtime)
    pure $
      concat
        [ concat statements,
          concat (concat classStatements),
          concat (concat dataStatements),
          imports,
          builtin
        ]
