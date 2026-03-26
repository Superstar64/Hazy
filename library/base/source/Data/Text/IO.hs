module Data.Text.IO
  ( readFile,
    writeFile,
    appendFile,
    hGetContents,
    hGetLine,
    hPutStr,
    hPutStrLn,
    interact,
    getContents,
    getLine,
    putStr,
    putStrLn,
  )
where

import Data.Text (Text)
import Hazy.Prelude (placeholder, putStrLn)
import System.IO (Handle)
import Prelude (FilePath, IO, error)

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
