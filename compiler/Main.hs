module Main (main) where

import Control.Exception (catch)
import Data.Char (toUpper)
import Data.Foldable (fold, for_, toList, traverse_)
import Data.Functor.Identity (Identity (..))
import Data.List.Reverse (List (..))
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text.IO (hGetContents)
import qualified Data.Text.Lazy as Lazy.Text
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
import qualified Stage1.Tree.Module as Stage1 (Module, assumeName, name)
import qualified Stage1.Variable as Variable
import qualified Stage2.Tree.Module as Module (resolve)
import qualified Stage2.Tree.Module as Stage2 (Module, name)
import qualified Stage3.Tree.Module as Module (check)
import qualified Stage3.Tree.Module as Stage3 (Module, name)
import qualified Stage4.Tree.Module as Module (simplify)
import qualified Stage4.Tree.Module as Stage4 (Module, name)
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

stage1 :: Debug -> Vector Loaded -> IO (Vector (Stage1.Module Position))
stage1 verbose = case verbose of
  Debug -> runVerbose . traverse parse
  Normal -> pure . runIdentity . traverse parse
  where
    parse Root {name, contents} = Parser.parse Module.parse name contents
    parse Inner {path, name, contents} = Stage1.assumeName path <$> Parser.parse Module.parse name contents

stage2 :: Debug -> Vector (Stage1.Module Position) -> IO (Vector Stage2.Module)
stage2 verbose = case verbose of
  Debug -> runVerbose . Module.resolve
  Normal -> pure . runIdentity . Module.resolve

stage3 :: Debug -> Vector Stage2.Module -> IO (Vector Stage3.Module)
stage3 _ = pure . Module.check

stage4 :: Debug -> Vector Stage3.Module -> IO (Vector Stage4.Module)
stage4 _ = pure . fmap Module.simplify

stage5 :: Debug -> Vector Stage4.Module -> IO (Vector (FullQualifiers, [Javascript.Statement 'False]))
stage5 _ = pure . Module.generate

message :: String -> Int -> Int -> FullQualifiers -> IO ()
message stage index total name =
  putStrLn $
    mconcat
      [ "[",
        Prelude.show index,
        " of ",
        Prelude.show total,
        "] ",
        stage,
        " ",
        Lazy.Text.unpack $ Builder.toLazyText $ Variable.print' name
      ]

forceModules :: (Foldable t, Prelude.Show a) => String -> Show -> Verbose -> (a -> FullQualifiers) -> t a -> IO ()
forceModules stage show verbose name modules = traverse_ go $ zip [1 ..] (toList modules)
  where
    total = length modules
    go (index, modulex) = do
      case verbose of
        Loud -> message stage index total (name modulex)
        Quiet -> pure ()
      case show of
        NoShow -> do
          -- todo, implement deepseq
          -- this is a horrible hack
          () <- pure $ seq (length (Prelude.show modulex)) ()
          pure ()
        Show -> do
          print modulex

data Mode
  = Parse
  | Resolve
  | Check
  | Simplify
  | Generate FilePath

data Verbose
  = Quiet
  | Loud

data Debug
  = Normal
  | Debug

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
    debug :: !Debug,
    failure :: !Failure,
    show :: !Show
  }

defaultx :: Execute
defaultx =
  Execute
    { include = Nil,
      modules = Nil,
      mode = Check,
      verbose = Loud,
      debug = Normal,
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
    Option [] ["debug-message"] (NoArg debug) "Show debug messages",
    Option [] ["debug-show"] (NoArg show) "Show Internal AST",
    Option [] ["fail"] (ReqArg fail "ERROR") "Except failure",
    Option ['I'] [] (ReqArg include "PATH") "Include path",
    Option ['q'] [] (NoArg quiet) "Don't show messages when compiling"
  ]
  where
    parse execute = execute {mode = Parse}
    resolve execute = execute {mode = Resolve}
    check execute = execute {mode = Check}
    simplify execute = execute {mode = Simplify}
    generate path execute = execute {mode = Generate path}
    debug execute = execute {debug = Debug}
    fail error execute = execute {failure = Expect error}
    include path execute@Execute {include} = execute {include = include :> path}
    show execute = execute {show = Show}
    quiet execute = execute {verbose = Quiet}

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
    let Execute {include, modules, mode, verbose, debug, failure, show} =
          foldl (flip id) defaultx flags
    include <- loadAllModules (toList include)
    modules <- loadAllModules (toList modules)
    let split = length include
        all = include <> modules
        run = case mode of
          Parse -> do
            all <- stage1 debug all
            forceModules "Parsing" show verbose Stage1.name (Vector.drop split all)
          Resolve -> do
            all <- stage1 debug all
            all <- stage2 debug all
            forceModules "Resolving" show verbose Stage2.name (Vector.drop split all)
          Check -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage3 debug all
            forceModules "Checking" show verbose Stage3.name (Vector.drop split all)
          Simplify -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage3 debug all
            all <- stage4 debug all
            forceModules "Simplifying" show verbose Stage4.name (Vector.drop split all)
          Generate target -> do
            all <- stage1 debug all
            all <- stage2 debug all
            all <- stage3 debug all
            all <- stage4 debug all
            all <- stage5 debug all
            let code = Vector.drop split all
                total = length code
            for_ (zip [1 ..] $ toList code) $ \(index, (name, statements)) -> do
              case verbose of
                Loud -> message "Compiling" index total name
                Quiet -> pure ()
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
        | Normal <- debug -> catch run Error.fail
        | Debug <- debug -> run
      Expect error
        | Just typex <- Map.lookup error Error.types ->
            catch (run >> noFail) (Error.allow typex)
        | otherwise -> putStrLn "Unknown error type" >> exitFailure
