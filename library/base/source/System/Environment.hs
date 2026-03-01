module System.Environment
  ( getArgs,
    getProgName,
    executablePath,
    getExecutablePath,
    getEnv,
    lookupEnv,
    setEnv,
    unsetEnv,
    withArgs,
    withProgName,
    getEnvironment,
  )
where

import Hazy (placeholder)

getArgs :: IO [String]
getArgs = placeholder

getProgName :: IO String
getProgName = placeholder

executablePath :: Maybe (IO (Maybe FilePath))
executablePath = placeholder

getExecutablePath :: IO FilePath
getExecutablePath = placeholder

getEnv :: String -> IO String
getEnv = placeholder

lookupEnv :: String -> IO (Maybe String)
lookupEnv = placeholder

setEnv :: String -> String -> IO ()
setEnv = placeholder

unsetEnv :: String -> IO ()
unsetEnv = placeholder

withArgs :: [String] -> IO a -> IO a
withArgs = placeholder

withProgName :: String -> IO a -> IO a
withProgName = placeholder

getEnvironment :: IO [(String, String)]
getEnvironment = placeholder
