module Data.Text.Lazy.IO where

import Data.Text.Lazy (Text)
import Hazy.Prelude (placeholder)
import System.IO (Handle)

readFile :: FilePath -> IO Text
readFile = placeholder

writeFile :: FilePath -> Text -> IO ()
writeFile = placeholder

appendFile :: FilePath -> Text -> IO ()
appendFile = placeholder

hGetContents :: Handle -> IO Text
hGetContents = placeholder

hGetLine :: Handle -> IO Text
hGetLine = placeholder

hPutStr :: Handle -> Text -> IO ()
hPutStr = placeholder

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn = placeholder

interact :: (Text -> Text) -> IO ()
interact = placeholder

getContents :: IO Text
getContents = placeholder

getLine :: IO Text
getLine = placeholder

putStr :: Text -> IO ()
putStr = placeholder

putStrLn :: Text -> IO ()
putStrLn = placeholder
