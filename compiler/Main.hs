module Main where

import Control.Exception (catch)
import Data.Char (toUpper)
import Data.Foldable (fold, for_, toList, traverse_)
import Data.Functor.Identity (Identity (..))
import Data.List.Reverse (List (..))
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text.IO (hGetContents)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Error (allow, fail, types)
import qualified Javascript.Printer.Lexer as Javascript (print, run)
import qualified Javascript.Tree.Module as Module (print)
import qualified Javascript.Tree.Statement as Javascript (Statement)
import Stage1.Lexer
  ( FullQualifiers (..),
    Qualifiers (..),
    constructorIdentifier,
  )
import qualified Stage1.Parser as Parser (parse)
import Stage1.Position (Position)
import qualified Stage1.Tree.Module as Module (parse)
import qualified Stage1.Tree.Module as Stage1 (Module, assumeName)
import qualified Stage2.Tree.Module as Module (resolve)
import qualified Stage2.Tree.Module as Stage2 (Module)
import qualified Stage3.Tree.Module as Module (check)
import qualified Stage3.Tree.Module as Stage3 (Module)
import qualified Stage4.Tree.Module as Module (simplify)
import qualified Stage4.Tree.Module as Stage4
import qualified Stage5.Generate.Mangle as Mangle
import qualified Stage5.Tree.Module as Module (generate)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, dropFileName, takeExtension, (</>))
import System.IO (IOMode (..), hSetEncoding, openFile, utf8)
import System.IO.Unsafe (unsafeInterleaveIO)
import Verbose (runVerbose)
import Prelude hiding (Show (show))
import qualified Prelude

data Loaded
  = Root
      { name :: !Text,
        contents :: Text
      }
  | Inner
      { path :: !FullQualifiers,
        name :: !Text,
        contents :: Text
      }

loadModule :: FilePath -> IO Text
loadModule file = unsafeInterleaveIO $ do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  Text.IO.hGetContents handle

loadModules :: FilePath -> IO (Vector Loaded)
loadModules path = Vector.fromList <$> loadModules [] path
  where
    loadModules :: [FilePath] -> FilePath -> IO [Loaded]
    loadModules modulePath file = case takeExtension file of
      "" -> do
        children <- listDirectory file
        let normalize (h : t) = dropExtension (toUpper h : t)
            normalize [] = error "empty path"
            recurse name = loadModules (normalize name : modulePath) (file </> name)
        inner <- traverse recurse children
        pure (concat inner)
      ".hs" ->
        let qualifiers = foldl (:.) Local (constructorIdentifier . pack <$> reverse modulePath)
         in case qualifiers of
              (modulePath :. moduleName)
                | path <- modulePath :.. moduleName -> do
                    contents <- loadModule file
                    pure [Inner {path, name = pack file, contents}]
              _ -> do
                contents <- loadModule file
                pure [Root {name = pack file, contents}]
      _ -> pure []

loadAllModules :: [FilePath] -> IO (Vector Loaded)
loadAllModules paths = fold <$> traverse loadModules paths

stage1 :: Verbose -> Vector Loaded -> IO (Vector (Stage1.Module Position))
stage1 verbose = case verbose of
  Loud -> runVerbose . traverse parse
  Quiet -> pure . runIdentity . traverse parse
  where
    parse Root {name, contents} = Parser.parse Module.parse name contents
    parse Inner {path, name, contents} = Stage1.assumeName path <$> Parser.parse Module.parse name contents

stage2 :: Verbose -> Vector (Stage1.Module Position) -> IO (Vector Stage2.Module)
stage2 verbose = case verbose of
  Loud -> runVerbose . Module.resolve
  Quiet -> pure . runIdentity . Module.resolve

stage3 :: Verbose -> Vector Stage2.Module -> IO (Vector Stage3.Module)
stage3 _ = pure . Module.check

stage4 :: Verbose -> Vector Stage3.Module -> IO (Vector Stage4.Module)
stage4 _ = pure . fmap Module.simplify

stage5 :: Verbose -> Vector Stage4.Module -> IO (Vector (FullQualifiers, [Javascript.Statement 'False]))
stage5 _ = pure . Module.generate

forceModules :: (Foldable t, Prelude.Show a) => t a -> ()
forceModules = foldr (seq . length . Prelude.show) ()

forceModulesM :: (Foldable t, Prelude.Show a) => Show -> t a -> IO ()
forceModulesM NoShow modules = do
  () <- pure $ forceModules modules
  pure ()
forceModulesM Show modules = traverse_ print modules

data Mode
  = Parse
  | Resolve
  | Check
  | Simplify
  | Generate FilePath

data Verbose
  = Quiet
  | Loud

data Show
  = Show
  | NoShow

data Failure
  = Attempt
  | Expect String

data Execute = Execute
  { include :: List String,
    modules :: List String,
    mode :: !Mode,
    verbose :: !Verbose,
    failure :: !Failure,
    show :: !Show
  }

defaultx :: Execute
defaultx =
  Execute
    { include = Nil,
      modules = Nil,
      mode = Check,
      verbose = Quiet,
      failure = Attempt,
      show = NoShow
    }

order :: ArgOrder (Execute -> Execute)
order = ReturnInOrder wrap
  where
    wrap path execute@Execute {modules} = execute {modules = modules :> path}

options :: [OptDescr (Execute -> Execute)]
options =
  [ Option [] ["parse"] (NoArg parse) "Parse source",
    Option [] ["resolve"] (NoArg resolve) "Resolve source",
    Option [] ["check"] (NoArg check) "Check source file",
    Option [] ["simplify"] (NoArg simplify) "Simplify source",
    Option ['o'] ["generate"] (ReqArg generate "PATH") "Generate Javascript from source file",
    Option [] ["debug"] (NoArg verbose) "Debug verbosity",
    Option [] ["show"] (NoArg show) "Show Internal AST",
    Option [] ["fail"] (ReqArg fail "ERROR") "Except failure",
    Option ['I'] [] (ReqArg include "PATH") "Include path"
  ]
  where
    parse execute = execute {mode = Parse}
    resolve execute = execute {mode = Resolve}
    check execute = execute {mode = Check}
    simplify execute = execute {mode = Simplify}
    generate path execute = execute {mode = Generate path}
    verbose execute = execute {verbose = Loud}
    fail error execute = execute {failure = Expect error}
    include path execute@Execute {include} = execute {include = include :> path}
    show execute = execute {show = Show}

main :: IO ()
main = getArgs >>= main''

main' :: String -> IO ()
main' args = main'' (words args)

main'' :: [String] -> IO ()
main'' args = case getOpt order options args of
  (_, _, errors@(_ : _)) -> do
    traverse_ putStrLn errors
    exitFailure
  (_, _ : _, _) -> error "flags not processed"
  (flags, [], []) -> do
    let Execute {include, modules, mode, verbose, failure, show} =
          foldl (flip id) defaultx flags
    include <- loadAllModules (toList include)
    modules <- loadAllModules (toList modules)
    let split = length include
        all = include <> modules
        run = case mode of
          Parse -> do
            all <- stage1 verbose all
            forceModulesM show (Vector.drop split all)
          Resolve -> do
            all <- stage1 verbose all
            all <- stage2 verbose all
            forceModulesM show (Vector.drop split all)
          Check -> do
            all <- stage1 verbose all
            all <- stage2 verbose all
            all <- stage3 verbose all
            forceModulesM show (Vector.drop split all)
          Simplify -> do
            all <- stage1 verbose all
            all <- stage2 verbose all
            all <- stage3 verbose all
            all <- stage4 verbose all
            forceModulesM show (Vector.drop split all)
          Generate target -> do
            all <- stage1 verbose all
            all <- stage2 verbose all
            all <- stage3 verbose all
            all <- stage4 verbose all
            all <- stage5 verbose all
            for_ (Vector.drop split all) $ \(name, statements) -> do
              let file = target </> Mangle.path name
              createDirectoryIfMissing True (dropFileName file)
              let javascript = Javascript.print $ Module.print statements
              let text = Builder.toLazyText $ Javascript.run javascript
              Text.Lazy.IO.writeFile file text
        noFail = do
          putStrLn "Code didn't fail"
          exitFailure
    case failure of
      Attempt
        | Quiet <- verbose -> catch run Error.fail
        | Loud <- verbose -> run
      Expect error
        | Just typex <- Map.lookup error Error.types ->
            catch (run >> noFail) (Error.allow typex)
        | otherwise -> putStrLn "Unknown error type" >> exitFailure
