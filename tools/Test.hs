{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Traversable (for)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (splitExtension, (-<.>), (</>))
import System.Process (callCommand, callProcess)

find :: String -> IO [String]
find path = do
  names <- listDirectory path
  fmap concat $ for names $ \name -> do
    directory <- doesDirectoryExist (path </> name)
    if
      | directory -> find (path </> name)
      | (_, ".hs") <- splitExtension name -> pure [path </> name]
      | otherwise -> pure []

callProcessVerbose :: String -> [String] -> IO ()
callProcessVerbose process arguments = do
  putStrLn $ process ++ " " ++ unwords arguments
  callProcess process arguments

callCommandVerbose :: String -> IO ()
callCommandVerbose command = do
  putStrLn command
  callCommand command

parse = do
  bad <- find "test/bad/parse"
  for_ bad $ \bad -> do
    callProcessVerbose "hazy" ["--fail", "Expected", "--parse", bad]

resolve = do
  bad <- find "test/bad/resolve"
  for_ bad $ \bad -> do
    let (base, _) = splitExtension bad
    fail <- head . lines <$> readFile (base ++ ".test")
    callProcessVerbose "hazy" ["--fail", fail, "--resolve", bad, "-I", "library/runtime", "-I", "library/base"]

library = do
  callProcessVerbose "hazy" ["--check", "-I", "library/runtime", "library/base"]

check = do
  bad <- find "test/bad/check"
  for_ bad $ \bad -> do
    fail <- head . lines <$> readFile (bad -<.> ".test")
    callProcessVerbose "hazy" ["--fail", fail, "--check", bad, "-I", "library/runtime", "-I", "library/base"]

good = do
  good <- find "test/good"
  for_ good $ \good ->
    callProcessVerbose "hazy" ["--simplify", good, "-I", "library/runtime", "-I", "library/base"]

run = do
  dirty <- doesDirectoryExist ".build"
  when dirty $ callProcessVerbose "rm" ["-r", ".build"]
  callProcessVerbose "mkdir" [".build"]
  callProcessVerbose "cp" ["-R", "runtime", ".build/base"]
  callProcessVerbose "hazy" ["-I", "library/runtime", "library/base", "-o", ".build/base"]
  run <- listDirectory "test/run"
  for_ run $ \run -> do
    callProcessVerbose "cp" ["-R", ".build/base", ".build/" ++ run]
    callProcessVerbose
      "hazy"
      [ "-I",
        "library/runtime",
        "-I",
        "library/base",
        "test/run/" ++ run ++ "/source",
        "-o",
        ".build/" ++ run
      ]
    callCommandVerbose $ "node .build/" ++ run ++ "/index.mjs > .build/" ++ run ++ "/result"
    callProcessVerbose "diff" ["test/run/" ++ run ++ "/result", ".build/" ++ run ++ "/result"]

main = do
  parse
  resolve
  library
  check
  good
  run
