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
import Hazy (IO)
import Text.Show (Show)
import Prelude (Integer, Read, error)

type FilePath = String

fixIO :: (a -> IO a) -> IO a
fixIO = error "todo"

data Handle

stdin :: Handle
stdin = error "todo"

stdout :: Handle
stdout = error "todo"

stderr :: Handle
stderr = error "todo"

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = error "todo"

openFile :: FilePath -> IOMode -> IO Handle
openFile = error "todo"

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode

hClose :: Handle -> IO ()
hClose = error "todo"

readFile :: FilePath -> IO String
readFile = error "todo"

readFile' :: FilePath -> IO String
readFile' = error "todo"

writeFile :: FilePath -> String -> IO ()
writeFile = error "todo"

appendFile :: FilePath -> String -> IO ()
appendFile = error "todo"

hFileSize :: Handle -> IO Integer
hFileSize = error "todo"

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize = error "todo"

hIsEOF :: Handle -> IO Bool
hIsEOF = error "todo"

isEOF :: IO Bool
isEOF = error "todo"

data BufferMode
  = NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering = error "todo"

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering = error "todo"

hFlush :: Handle -> IO ()
hFlush = error "todo"

hGetPosn :: Handle -> IO HandlePosn
hGetPosn = error "todo"

hSetPosn :: HandlePosn -> IO ()
hSetPosn = error "todo"

data HandlePosn

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek = error "todo"

data SeekMode
  = AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd

hTell :: Handle -> IO Integer
hTell = error "todo"

hIsOpen :: Handle -> IO Bool
hIsOpen = error "todo"

hIsClosed :: Handle -> IO Bool
hIsClosed = error "todo"

hIsReadable :: Handle -> IO Bool
hIsReadable = error "todo"

hIsWritable :: Handle -> IO Bool
hIsWritable = error "todo"

hIsSeekable :: Handle -> IO Bool
hIsSeekable = error "todo"

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice = error "todo"

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho = error "todo"

hGetEcho :: Handle -> IO Bool
hGetEcho = error "todo"

hShow :: Handle -> IO String
hShow = error "todo"

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput = error "todo"

hReady :: Handle -> IO Bool
hReady = error "todo"

hGetChar :: Handle -> IO Char
hGetChar = error "todo"

hGetLine :: Handle -> IO String
hGetLine = error "todo"

hLookAhead :: Handle -> IO Char
hLookAhead = error "todo"

hGetContents :: Handle -> IO String
hGetContents = error "todo"

hGetContents' :: Handle -> IO String
hGetContents' = error "todo"

hPutChar :: Handle -> Char -> IO ()
hPutChar = error "todo"

hPutStr :: Handle -> String -> IO ()
hPutStr = error "todo"

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn = error "todo"

hPrint :: (Show a) => Handle -> a -> IO ()
hPrint = error "todo"

interact :: (String -> String) -> IO ()
interact = error "todo"

putChar :: Char -> IO ()
putChar = error "todo"

putStr :: String -> IO ()
putStr = error "todo"

putStrLn :: String -> IO ()
putStrLn string = Text.putStrLn (pack string)

print :: (Show a) => a -> IO ()
print = error "todo"

getChar :: IO Char
getChar = error "todo"

getLine :: IO String
getLine = error "todo"

getContents :: IO String
getContents = error "todo"

readIO :: (Read a) => String -> IO a
readIO = error "todo"

readLn :: (Read a) => IO a
readLn = error "todo"

getContents' :: IO String
getContents' = error "todo"

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = error "todo"

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = error "todo"

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode = error "todo"

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf = error "todo"

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf = error "todo"

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome = error "todo"

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking = error "todo"

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = error "todo"

openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile = error "todo"

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = error "todo"

openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions = error "todo"

openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions = error "todo"

hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding = error "todo"

hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding = error "todo"

data TextEncoding

latin1 :: TextEncoding
latin1 = error "todo"

utf8 :: TextEncoding
utf8 = error "todo"

utf8_bom :: TextEncoding
utf8_bom = error "todo"

utf16 :: TextEncoding
utf16 = error "todo"

utf16le :: TextEncoding
utf16le = error "todo"

utf16be :: TextEncoding
utf16be = error "todo"

utf32 :: TextEncoding
utf32 = error "todo"

utf32le :: TextEncoding
utf32le = error "todo"

utf32be :: TextEncoding
utf32be = error "todo"

localeEncoding :: TextEncoding
localeEncoding = error "todo"

char8 :: TextEncoding
char8 = error "todo"

mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding = error "todo"

hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetNewlineMode = error "todo"

data Newline
  = LF
  | CRLF

nativeNewline :: Newline
nativeNewline = error "todo"

data NewlineMode = NewlineMode
  { inputNL :: Newline,
    outputNL :: Newline
  }

noNewlineTranslation :: NewlineMode
noNewlineTranslation = error "todo"

universalNewlineMode :: NewlineMode
universalNewlineMode = error "todo"

nativeNewlineMode :: NewlineMode
nativeNewlineMode = error "todo"
