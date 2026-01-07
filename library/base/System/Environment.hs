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

getArgs :: IO [String]
getArgs = error "todo"

getProgName :: IO String
getProgName = error "todo"

executablePath :: Maybe (IO (Maybe FilePath))
executablePath = error "todo"

getExecutablePath :: IO FilePath
getExecutablePath = error "todo"

getEnv :: String -> IO String
getEnv = error "todo"

lookupEnv :: String -> IO (Maybe String)
lookupEnv = error "todo"

setEnv :: String -> String -> IO ()
setEnv = error "todo"

unsetEnv :: String -> IO ()
unsetEnv = error "todo"

withArgs :: [String] -> IO a -> IO a
withArgs = error "todo"

withProgName :: String -> IO a -> IO a
withProgName = error "todo"

getEnvironment :: IO [(String, String)]
getEnvironment = error "todo"
