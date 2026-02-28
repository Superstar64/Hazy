{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Traversable (for)
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    findExecutable,
    listDirectory,
    removeDirectoryRecursive,
  )
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

parse hazy = do
  bad <- find "test/bad/parse"
  for_ bad $ \bad -> do
    callProcessVerbose hazy ["--debug-fail", "Expected", "--parse", bad]

resolve hazy = do
  bad <- find "test/bad/resolve"
  for_ bad $ \bad -> do
    let (base, _) = splitExtension bad
    fail <- head . lines <$> readFile (base ++ ".test")
    callProcessVerbose hazy ["--debug-fail", fail, "--resolve", bad]

check hazy = do
  bad <- find "test/bad/check"
  for_ bad $ \bad -> do
    fail <- head . lines <$> readFile (bad -<.> ".test")
    callProcessVerbose hazy ["--debug-fail", fail, "--check", bad]

good hazy = do
  good <- find "test/good"
  for_ good $ \good ->
    callProcessVerbose hazy ["--debug-simplify", good]

run hazy = do
  run <- listDirectory "test/run"
  for_ run $ \run -> do
    callProcessVerbose hazy ["test/run/" ++ run ++ "/source", "-o", ".build/" ++ run]
    callCommandVerbose $ "node .build/" ++ run ++ "/index.mjs > .build/" ++ run ++ "/result"
    callProcessVerbose "diff" ["test/run/" ++ run ++ "/result", ".build/" ++ run ++ "/result"]

main = do
  Just source <- findExecutable "hazy"
  let hazy = ".build/dist/bin/hazy"
  dirty <- doesDirectoryExist ".build"
  when dirty $ removeDirectoryRecursive ".build"
  createDirectoryIfMissing False ".build"
  createDirectory ".build/dist"
  createDirectory ".build/dist/bin"
  createDirectory ".build/dist/packages"
  copyFile source hazy
  -- todo, use proper recursive copy in Haskell
  callProcess "cp" ["-R", "library/runtime/", ".build/dist/packages/runtime"]
  flags <- readFile "library/base/flags"
  callProcessVerbose
    hazy
    $ [ "--bare-runtime",
        "--pack",
        "library/base/source",
        "-o",
        ".build/dist/packages/base"
      ]
      ++ words flags
  parse hazy
  resolve hazy
  check hazy
  good hazy
  run hazy
