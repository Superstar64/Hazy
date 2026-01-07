module Data.Text.Lazy.IO where

import Data.Text.Lazy (Text)
import System.IO (Handle)

readFile :: FilePath -> IO Text
readFile = error "todo"

writeFile :: FilePath -> Text -> IO ()
writeFile = error "todo"

appendFile :: FilePath -> Text -> IO ()
appendFile = error "todo"

hGetContents :: Handle -> IO Text
hGetContents = error "todo"

hGetLine :: Handle -> IO Text
hGetLine = error "todo"

hPutStr :: Handle -> Text -> IO ()
hPutStr = error "todo"

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn = error "todo"

interact :: (Text -> Text) -> IO ()
interact = error "todo"

getContents :: IO Text
getContents = error "todo"

getLine :: IO Text
getLine = error "todo"

putStr :: Text -> IO ()
putStr = error "todo"

putStrLn :: Text -> IO ()
putStrLn = error "todo"
