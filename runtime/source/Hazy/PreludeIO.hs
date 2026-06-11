module Hazy.PreludeIO where

import Hazy.Prelude

type FilePath = String

data IOError

instance Show IOError

instance Eq IOError

ioError :: IOError -> IO a
ioError = placeholder

userError :: String -> IOError
userError = placeholder

catch :: IO a -> (IOError -> IO a) -> IO a
catch = placeholder

putChar :: Char -> IO ()
putChar = placeholder

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn = putStrLnText . pack

print :: (Show a) => a -> IO ()
print x = putStrLn (show x)

getChar :: IO Char
getChar = placeholder

getLine :: IO String
getLine = do
  c <- getChar
  if c == '\n'
    then return ""
    else do
      s <- getLine
      return (c : s)

getContents :: IO String
getContents = placeholder

-- The haskell's report definition has missing variables
interact :: (String -> String) -> IO ()
interact f = placeholder

readFile :: FilePath -> IO String
readFile = placeholder

writeFile :: FilePath -> String -> IO ()
writeFile = placeholder

appendFile :: FilePath -> String -> IO ()
appendFile = placeholder

readIO :: (Read a) => String -> IO a
readIO s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> return x
  [] -> ioError (userError "Prelude.readIO: no parse")
  _ -> ioError (userError "Prelude.readIO: ambiguous parse")

readLn :: (Read a) => IO a
readLn = do
  l <- getLine
  r <- readIO l
  return r
