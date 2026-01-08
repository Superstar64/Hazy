module Stage5.Tree.Module where

import Control.Monad.ST (ST, runST)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.STRef (readSTRef)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Javascript.Tree.Statement as Javascript (Statement (..))
import Stage1.Lexer (FullQualifiers)
import Stage1.Variable (FullyQualifiedConstructorIdentifier ((:.=.)))
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Scope as Scope
import Stage4.Tree.Declarations (Declarations (..))
import Stage4.Tree.Module (Module (..))
import qualified Stage4.Tree.Scheme as Scheme
import Stage4.Tree.TermDeclaration (TermDeclaration (Definition))
import qualified Stage4.Tree.TermDeclaration as TermDeclaration
import qualified Stage4.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage5.Generate.Context as Context
import Stage5.Generate.Global (Global (Global))
import qualified Stage5.Generate.Global as Global
import Stage5.Generate.GlobalType (GlobalType (GlobalType))
import qualified Stage5.Generate.GlobalType as GlobalType
import qualified Stage5.Generate.Mangle as Mandle
import qualified Stage5.Generate.Mangle as Mangle
import Stage5.Generate.Precontext (Precontext (Precontext))
import qualified Stage5.Generate.Precontext as Precontext
import qualified Stage5.Tree.Expression as Expression
import qualified Stage5.Tree.Instance as Instance

generate :: Vector Module -> Vector (FullQualifiers, [Javascript.Statement 'False])
generate modules = Vector.imap go modules
  where
    pre = precontext modules
    go index Module {name, declarations} =
      let statements = runST (generate' pre name index declarations)
       in (name, statements)

precontext :: Vector Module -> Precontext
precontext modules =
  Precontext
    { terms = names <$> modules,
      types = instanceNames <$> modules
    }
  where
    names Module {name = path, declarations = Declarations {terms}} = generate <$> terms
      where
        generate Definition {name} =
          Global
            { path,
              name = Mangle.mangle name
            }
    instanceNames
      Module
        { name = path,
          declarations = Declarations {types, classInstances, dataInstances}
        } = Vector.zipWith3 generate types classInstances dataInstances
        where
          global name = Global {path, name}
          generate typex classInstances dataInstances =
            GlobalType
              { classInstances =
                  let go key _ = global $ single Mangle.Class key
                   in Map.mapWithKey go classInstances,
                dataInstances =
                  let go key _ = global $ single Mangle.Data key
                   in Map.mapWithKey go dataInstances
              }
            where
              name = TypeDeclaration.name typex
              single :: Mangle.Brand -> Type2.Index Scope.Global -> Text
              single brand = Mangle.mangleInstance target brand name
              target :: Type.Index Scope.Global -> FullyQualifiedConstructorIdentifier
              target (Type.Global global local) = targetPath :.=. targetName
                where
                  Module
                    { name = targetPath,
                      declarations = Declarations {types}
                    } = modules Vector.! global
                  targetName = TypeDeclaration.name $ types Vector.! local

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
    let (numInt, numInteger, enumInt, enumInteger, unique) = case Mandle.unique of
          (numInt : numInteger : enumInt : enumInteger : unique) ->
            (numInt, numInteger, enumInt, enumInteger, unique)
          _ -> undefined
        builtin =
          Context.Builtin
            { numInt,
              numInteger,
              enumInt,
              enumInteger
            }
    context <- Context.start precontext builtin unique
    statements <-
      for (zip [0 ..] (toList terms)) $
        \( termIndex,
           Definition
             { typex,
               definition
             }
           ) ->
            do
              let count = Scheme.constraintCount typex
              thunk <- Expression.declaration context count definition
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
    let builtin =
          [ Javascript.Import Mangle.numInt numInt (Mangle.depth moduleName <> Mangle.runtime),
            Javascript.Import Mangle.numInteger numInteger (Mangle.depth moduleName <> Mangle.runtime),
            Javascript.Import Mangle.enumInt enumInt (Mangle.depth moduleName <> Mangle.runtime),
            Javascript.Import Mangle.enumInteger enumInteger (Mangle.depth moduleName <> Mangle.runtime)
          ]
    pure $
      concat
        [ concat statements,
          concat (concat classStatements),
          concat (concat dataStatements),
          imports,
          builtin
        ]
