{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Traversable (for)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (splitExtension, (-<.>), (</>))
import System.Process (callProcess)

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

main = do
  parse
  resolve
  library
  check
  good
