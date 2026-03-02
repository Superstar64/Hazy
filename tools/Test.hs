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
import System.FilePath (splitExtension, takeFileName, (-<.>), (</>))
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
    callProcessVerbose hazy ["test/run/" ++ run ++ "/source", "-o", ".test/" ++ run]
    callCommandVerbose $ "node .test/" ++ run ++ "/index.mjs > .test/" ++ run ++ "/result"
    callProcessVerbose "diff" ["test/run/" ++ run ++ "/result", ".test/" ++ run ++ "/result"]

main = do
  Just source <- findExecutable "hazy"
  let hazy = ".test/dist/bin/" ++ takeFileName source
  dirty <- doesDirectoryExist ".test"
  when dirty $ removeDirectoryRecursive ".test"
  createDirectoryIfMissing False ".test"
  createDirectory ".test/dist"
  createDirectory ".test/dist/bin"
  createDirectory ".test/dist/packages"
  copyFile source hazy
  -- todo, use proper recursive copy in Haskell
  callProcess "cp" ["-R", "library/runtime/", ".test/dist/packages/runtime"]
  flags <- readFile "library/base/flags"
  callProcessVerbose
    hazy
    $ [ "--bare-runtime",
        "--pack",
        "library/base/source",
        "-o",
        ".test/dist/packages/base"
      ]
      ++ words flags
  parse hazy
  resolve hazy
  check hazy
  good hazy
  run hazy
