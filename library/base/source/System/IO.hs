module System.IO
  ( IO,
    fixIO,
    FilePath,
    Handle,
    stdin,
    stdout,
    stderr,
    withFile,
    openFile,
    IOMode (..),
    hClose,
    readFile,
    readFile',
    writeFile,
    appendFile,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    BufferMode (..),
    hSetBuffering,
    hGetBuffering,
    hFlush,
    hGetPosn,
    hSetPosn,
    HandlePosn (..),
    hSeek,
    SeekMode (..),
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hSetEcho,
    hGetEcho,
    hShow,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hLookAhead,
    hGetContents,
    hGetContents,
    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,
    interact,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    readIO,
    readLn,
    withBinaryFile,
    openBinaryFile,
    hSetBinaryMode,
    hPutBuf,
    hGetBuf,
    hGetBufSome,
    hPutBufNonBlocking,
    hGetBufNonBlocking,
    openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,
    hSetEncoding,
    hGetEncoding,
    TextEncoding (..),
    latin1,
    utf8,
    utf8_bom,
    utf16,
    utf16le,
    utf16be,
    utf32,
    utf32le,
    utf32be,
    localeEncoding,
    char8,
    mkTextEncoding,
    hSetNewlineMode,
    Newline (..),
    nativeNewline,
    NewlineMode (..),
    noNewlineTranslation,
    universalNewlineMode,
    nativeNewlineMode,
  )
where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.String (String)
import Data.Text (pack)
import qualified Data.Text.IO as Text
import Foreign.Ptr (Ptr)
import Hazy (IO, placeholder)
import Text.Show (Show)
import Prelude (Integer, Read, error)

type FilePath = String

fixIO :: (a -> IO a) -> IO a
fixIO = placeholder

data Handle

stdin :: Handle
stdin = placeholder

stdout :: Handle
stdout = placeholder

stderr :: Handle
stderr = placeholder

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = placeholder

openFile :: FilePath -> IOMode -> IO Handle
openFile = placeholder

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode

hClose :: Handle -> IO ()
hClose = placeholder

readFile :: FilePath -> IO String
readFile = placeholder

readFile' :: FilePath -> IO String
readFile' = placeholder

writeFile :: FilePath -> String -> IO ()
writeFile = placeholder

appendFile :: FilePath -> String -> IO ()
appendFile = placeholder

hFileSize :: Handle -> IO Integer
hFileSize = placeholder

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize = placeholder

hIsEOF :: Handle -> IO Bool
hIsEOF = placeholder

isEOF :: IO Bool
isEOF = placeholder

data BufferMode
  = NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering = placeholder

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering = placeholder

hFlush :: Handle -> IO ()
hFlush = placeholder

hGetPosn :: Handle -> IO HandlePosn
hGetPosn = placeholder

hSetPosn :: HandlePosn -> IO ()
hSetPosn = placeholder

data HandlePosn

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek = placeholder

data SeekMode
  = AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd

hTell :: Handle -> IO Integer
hTell = placeholder

hIsOpen :: Handle -> IO Bool
hIsOpen = placeholder

hIsClosed :: Handle -> IO Bool
hIsClosed = placeholder

hIsReadable :: Handle -> IO Bool
hIsReadable = placeholder

hIsWritable :: Handle -> IO Bool
hIsWritable = placeholder

hIsSeekable :: Handle -> IO Bool
hIsSeekable = placeholder

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice = placeholder

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho = placeholder

hGetEcho :: Handle -> IO Bool
hGetEcho = placeholder

hShow :: Handle -> IO String
hShow = placeholder

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput = placeholder

hReady :: Handle -> IO Bool
hReady = placeholder

hGetChar :: Handle -> IO Char
hGetChar = placeholder

hGetLine :: Handle -> IO String
hGetLine = placeholder

hLookAhead :: Handle -> IO Char
hLookAhead = placeholder

hGetContents :: Handle -> IO String
hGetContents = placeholder

hGetContents' :: Handle -> IO String
hGetContents' = placeholder

hPutChar :: Handle -> Char -> IO ()
hPutChar = placeholder

hPutStr :: Handle -> String -> IO ()
hPutStr = placeholder

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn = placeholder

hPrint :: (Show a) => Handle -> a -> IO ()
hPrint = placeholder

interact :: (String -> String) -> IO ()
interact = placeholder

putChar :: Char -> IO ()
putChar = placeholder

putStr :: String -> IO ()
putStr = placeholder

putStrLn :: String -> IO ()
putStrLn string = Text.putStrLn (pack string)

print :: (Show a) => a -> IO ()
print = placeholder

getChar :: IO Char
getChar = placeholder

getLine :: IO String
getLine = placeholder

getContents :: IO String
getContents = placeholder

readIO :: (Read a) => String -> IO a
readIO = placeholder

readLn :: (Read a) => IO a
readLn = placeholder

getContents' :: IO String
getContents' = placeholder

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = placeholder

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = placeholder

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode = placeholder

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf = placeholder

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf = placeholder

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome = placeholder

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking = placeholder

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = placeholder

openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile = placeholder

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = placeholder

openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions = placeholder

openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions = placeholder

hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding = placeholder

hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding = placeholder

data TextEncoding

latin1 :: TextEncoding
latin1 = placeholder

utf8 :: TextEncoding
utf8 = placeholder

utf8_bom :: TextEncoding
utf8_bom = placeholder

utf16 :: TextEncoding
utf16 = placeholder

utf16le :: TextEncoding
utf16le = placeholder

utf16be :: TextEncoding
utf16be = placeholder

utf32 :: TextEncoding
utf32 = placeholder

utf32le :: TextEncoding
utf32le = placeholder

utf32be :: TextEncoding
utf32be = placeholder

localeEncoding :: TextEncoding
localeEncoding = placeholder

char8 :: TextEncoding
char8 = placeholder

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding = placeholder

hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetNewlineMode = placeholder

data Newline
  = LF
  | CRLF

nativeNewline :: Newline
nativeNewline = placeholder

data NewlineMode = NewlineMode
  { inputNL :: Newline,
    outputNL :: Newline
  }

noNewlineTranslation :: NewlineMode
noNewlineTranslation = placeholder

universalNewlineMode :: NewlineMode
universalNewlineMode = placeholder

nativeNewlineMode :: NewlineMode
nativeNewlineMode = placeholder
