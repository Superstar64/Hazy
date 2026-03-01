module System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    renameDirectory,
    listDirectory,
    getDirectoryContents,
    getCurrentDirectory,
    setCurrentDirectory,
    withCurrentDirectory,
    getHomeDirectory,
    XdgDirectory (..),
    getXdgDirectory,
    XdgDirectoryList (..),
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,
    removeFile,
    renameFile,
    renamePath,
    copyFile,
    copyFileWithMetadata,
    getFileSize,
    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    exeExtension,
    createFileLink,
    createDirectoryLink,
    removeDirectoryLink,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,
    Permissions,
    emptyPermissions,
    readable,
    writable,
    executable,
    searchable,
    setOwnerReadable,
    setOwnerWritable,
    setOwnerExecutable,
    setOwnerSearchable,
    getPermissions,
    setPermissions,
    copyPermissions,
    isSymbolicLink,
  )
where

import Hazy (placeholder)

createDirectory :: FilePath -> IO ()
createDirectory = placeholder

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing = placeholder

removeDirectory :: FilePath -> IO ()
removeDirectory = placeholder

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = placeholder

removePathForcibly :: FilePath -> IO ()
removePathForcibly = placeholder

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory = placeholder

listDirectory :: FilePath -> IO [FilePath]
listDirectory = placeholder

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = placeholder

getCurrentDirectory :: IO FilePath
getCurrentDirectory = placeholder

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = placeholder

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory = placeholder

getHomeDirectory :: IO FilePath
getHomeDirectory = placeholder

data XdgDirectory
  = XdgData
  | XdgConfig
  | XdgCache
  | XdgState

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory = placeholder

data XdgDirectoryList
  = XdgDataDirs
  | XdgConfigDirs

getXdgDirectoryList :: XdgDirectoryList -> IO [FilePath]
getXdgDirectoryList = placeholder

getAppUserDataDirectory :: FilePath -> IO FilePath
getAppUserDataDirectory = placeholder

getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = placeholder

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = placeholder

removeFile :: FilePath -> IO ()
removeFile = placeholder

renameFile :: FilePath -> FilePath -> IO ()
renameFile = placeholder

renamePath :: FilePath -> FilePath -> IO ()
renamePath = placeholder

copyFile :: FilePath -> FilePath -> IO ()
copyFile = placeholder

copyFileWithMetadata :: FilePath -> FilePath -> IO ()
copyFileWithMetadata = placeholder

getFileSize :: FilePath -> IO Integer
getFileSize = placeholder

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = placeholder

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = placeholder

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory = placeholder

doesPathExist :: FilePath -> IO Bool
doesPathExist = placeholder

doesFileExist :: FilePath -> IO Bool
doesFileExist = placeholder

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = placeholder

findExecutable :: String -> IO (Maybe FilePath)
findExecutable = placeholder

findExecutables :: String -> IO [FilePath]
findExecutables = placeholder

findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories = placeholder

findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile = placeholder

findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = placeholder

findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFileWith = placeholder

findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith = placeholder

exeExtension :: String
exeExtension = placeholder

createFileLink :: FilePath -> FilePath -> IO ()
createFileLink = placeholder

createDirectoryLink :: FilePath -> FilePath -> IO ()
createDirectoryLink = placeholder

removeDirectoryLink :: FilePath -> IO ()
removeDirectoryLink = placeholder

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink = placeholder

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = placeholder

data Permissions

emptyPermissions :: Permissions
emptyPermissions = placeholder

readable :: Permissions -> Bool
readable = placeholder

writable :: Permissions -> Bool
writable = placeholder

executable :: Permissions -> Bool
executable = placeholder

searchable :: Permissions -> Bool
searchable = placeholder

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable = placeholder

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable = placeholder

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable = placeholder

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable = placeholder

getPermissions :: FilePath -> IO Permissions
getPermissions = placeholder

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions = placeholder

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions = placeholder

isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = placeholder
