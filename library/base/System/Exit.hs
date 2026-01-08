{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module System.Exit where

data ExitCode
  = ExitSuccess
  | ExitFailure Int

exitWith :: ExitCode -> IO a
exitWith = error "todo"

exitFailure :: IO a
exitFailure = error "todo"

exitSuccess :: IO a
exitSuccess = error "todo"

die :: String -> IO a
die = error "todo"
