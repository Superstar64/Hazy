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

parse = do
  bad <- find "test/bad/parse"
  for_ bad $ \bad -> do
    callProcessVerbose "hazy" ["--bare", "--debug-fail", "Expected", "--parse", bad]

resolve = do
  bad <- find "test/bad/resolve"
  for_ bad $ \bad -> do
    let (base, _) = splitExtension bad
    fail <- head . lines <$> readFile (base ++ ".test")
    callProcessVerbose
      "hazy"
      [ "--bare",
        "--debug-fail",
        fail,
        "--resolve",
        bad,
        "-I",
        "library/runtime/header",
        "-I",
        "library/base"
      ]

library = do
  callProcessVerbose
    "hazy"
    [ "--bare",
      "--check",
      "-I",
      "library/runtime/header",
      "library/base"
    ]

check = do
  bad <- find "test/bad/check"
  for_ bad $ \bad -> do
    fail <- head . lines <$> readFile (bad -<.> ".test")
    callProcessVerbose
      "hazy"
      [ "--bare",
        "--debug-fail",
        fail,
        "--check",
        bad,
        "-I",
        "library/runtime/header",
        "-I",
        "library/base"
      ]

good = do
  good <- find "test/good"
  for_ good $ \good ->
    callProcessVerbose
      "hazy"
      [ "--bare",
        "--debug-simplify",
        good,
        "-I",
        "library/runtime/header",
        "-I",
        "library/base"
      ]

run = do
  Just hazy <- findExecutable "hazy"
  dirty <- doesDirectoryExist ".build"
  when dirty $ removeDirectoryRecursive ".build"
  createDirectoryIfMissing False ".build"
  createDirectory ".build/dist"
  createDirectory ".build/dist/bin"
  createDirectory ".build/dist/packages"
  copyFile hazy ".build/dist/bin/hazy"
  -- todo, use proper recursive copy in Haskell
  callProcess "cp" ["-R", "library/runtime/", ".build/dist/packages/runtime"]
  callProcessVerbose
    ".build/dist/bin/hazy"
    [ "--bare-runtime",
      "--pack",
      "library/base",
      "-o",
      ".build/dist/packages/base"
    ]

  run <- listDirectory "test/run"
  for_ run $ \run -> do
    callProcessVerbose ".build/dist/bin/hazy" ["test/run/" ++ run ++ "/source", "-o", ".build/" ++ run]
    callCommandVerbose $ "node .build/" ++ run ++ "/index.mjs > .build/" ++ run ++ "/result"
    callProcessVerbose "diff" ["test/run/" ++ run ++ "/result", ".build/" ++ run ++ "/result"]

main = do
  parse
  resolve
  library
  check
  good
  run
