module System.Exit where

import Hazy.Prelude (placeholder)

data ExitCode
  = ExitSuccess
  | ExitFailure Int

exitWith :: ExitCode -> IO a
exitWith = placeholder

exitFailure :: IO a
exitFailure = placeholder

exitSuccess :: IO a
exitSuccess = placeholder

die :: String -> IO a
die = placeholder
