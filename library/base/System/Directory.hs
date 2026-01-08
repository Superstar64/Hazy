{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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

createDirectory :: FilePath -> IO ()
createDirectory = error "todo"

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing = error "todo"

removeDirectory :: FilePath -> IO ()
removeDirectory = error "todo"

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = error "todo"

removePathForcibly :: FilePath -> IO ()
removePathForcibly = error "todo"

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory = error "todo"

listDirectory :: FilePath -> IO [FilePath]
listDirectory = error "todo"

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = error "todo"

getCurrentDirectory :: IO FilePath
getCurrentDirectory = error "todo"

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = error "todo"

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory = error "todo"

getHomeDirectory :: IO FilePath
getHomeDirectory = error "todo"

data XdgDirectory
  = XdgData
  | XdgConfig
  | XdgCache
  | XdgState

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory = error "todo"

data XdgDirectoryList
  = XdgDataDirs
  | XdgConfigDirs

getXdgDirectoryList :: XdgDirectoryList -> IO [FilePath]
getXdgDirectoryList = error "todo"

getAppUserDataDirectory :: FilePath -> IO FilePath
getAppUserDataDirectory = error "todo"

getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = error "todo"

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = error "todo"

removeFile :: FilePath -> IO ()
removeFile = error "todo"

renameFile :: FilePath -> FilePath -> IO ()
renameFile = error "todo"

renamePath :: FilePath -> FilePath -> IO ()
renamePath = error "todo"

copyFile :: FilePath -> FilePath -> IO ()
copyFile = error "todo"

copyFileWithMetadata :: FilePath -> FilePath -> IO ()
copyFileWithMetadata = error "todo"

getFileSize :: FilePath -> IO Integer
getFileSize = error "todo"

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = error "todo"

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = error "todo"

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory = error "todo"

doesPathExist :: FilePath -> IO Bool
doesPathExist = error "todo"

doesFileExist :: FilePath -> IO Bool
doesFileExist = error "todo"

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = error "todo"

findExecutable :: String -> IO (Maybe FilePath)
findExecutable = error "todo"

findExecutables :: String -> IO [FilePath]
findExecutables = error "todo"

findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories = error "todo"

findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile = error "todo"

findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = error "todo"

findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFileWith = error "todo"

findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith = error "todo"

exeExtension :: String
exeExtension = error "todo"

createFileLink :: FilePath -> FilePath -> IO ()
createFileLink = error "todo"

createDirectoryLink :: FilePath -> FilePath -> IO ()
createDirectoryLink = error "todo"

removeDirectoryLink :: FilePath -> IO ()
removeDirectoryLink = error "todo"

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink = error "todo"

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = error "todo"

data Permissions

emptyPermissions :: Permissions
emptyPermissions = error "todo"

readable :: Permissions -> Bool
readable = error "todo"

writable :: Permissions -> Bool
writable = error "todo"

executable :: Permissions -> Bool
executable = error "todo"

searchable :: Permissions -> Bool
searchable = error "todo"

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable = error "todo"

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable = error "todo"

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable = error "todo"

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable = error "todo"

getPermissions :: FilePath -> IO Permissions
getPermissions = error "todo"

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions = error "todo"

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions = error "todo"

isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = error "todo"
