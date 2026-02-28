module System.FilePath
  ( FilePath,
    pathSeparator,
    pathSeparators,
    isPathSeparator,
    searchPathSeparator,
    isSearchPathSeparator,
    extSeparator,
    isExtSeparator,
    splitSearchPath,
    getSearchPath,
    splitExtension,
    takeExtension,
    replaceExtension,
    (-<.>),
    dropExtension,
    addExtension,
    hasExtension,
    (<.>),
    splitExtensions,
    dropExtensions,
    takeExtensions,
    replaceExtensions,
    isExtensionOf,
    stripExtension,
    splitFileName,
    takeFileName,
    replaceFileName,
    dropFileName,
    takeBaseName,
    replaceBaseName,
    takeDirectory,
    replaceDirectory,
    combine,
    (</>),
    splitPath,
    joinPath,
    splitDirectories,
    splitDrive,
    joinDrive,
    takeDrive,
    hasDrive,
    dropDrive,
    isDrive,
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,
    normalise,
    equalFilePath,
    makeRelative,
    isRelative,
    isAbsolute,
    isValid,
    makeValid,
  )
where

pathSeparator :: Char
pathSeparator = error "todo"

pathSeparators :: [Char]
pathSeparators = error "todo"

isPathSeparator :: Char -> Bool
isPathSeparator = error "todo"

searchPathSeparator :: Char
searchPathSeparator = error "todo"

isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = error "todo"

extSeparator :: Char
extSeparator = error "todo"

isExtSeparator :: Char -> Bool
isExtSeparator = error "todo"

splitSearchPath :: String -> [FilePath]
splitSearchPath = error "todo"

getSearchPath :: IO [FilePath]
getSearchPath = error "todo"

splitExtension :: FilePath -> (String, String)
splitExtension = error "todo"

takeExtension :: FilePath -> String
takeExtension = error "todo"

replaceExtension :: FilePath -> String -> FilePath
replaceExtension = error "todo"

infixr 7 -<.>

(-<.>) :: FilePath -> String -> FilePath
(-<.>) = error "todo"

dropExtension :: FilePath -> FilePath
dropExtension = error "todo"

addExtension :: FilePath -> String -> FilePath
addExtension = error "todo"

hasExtension :: FilePath -> Bool
hasExtension = error "todo"

infixr 7 <.>

(<.>) :: FilePath -> String -> FilePath
(<.>) = error "todo"

splitExtensions :: FilePath -> (FilePath, String)
splitExtensions = error "todo"

dropExtensions :: FilePath -> FilePath
dropExtensions = error "todo"

takeExtensions :: FilePath -> String
takeExtensions = error "todo"

replaceExtensions :: FilePath -> String -> FilePath
replaceExtensions = error "todo"

isExtensionOf :: String -> FilePath -> Bool
isExtensionOf = error "todo"

stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension = error "todo"

splitFileName :: FilePath -> (String, String)
splitFileName = error "todo"

takeFileName :: FilePath -> FilePath
takeFileName = error "todo"

replaceFileName :: FilePath -> String -> FilePath
replaceFileName = error "todo"

dropFileName :: FilePath -> FilePath
dropFileName = error "todo"

takeBaseName :: FilePath -> String
takeBaseName = error "todo"

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName = error "todo"

takeDirectory :: FilePath -> FilePath
takeDirectory = error "todo"

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory = error "todo"

combine :: FilePath -> FilePath -> FilePath
combine = error "todo"

infixr 5 </>

(</>) :: FilePath -> FilePath -> FilePath
(</>) = error "todo"

splitPath :: FilePath -> [FilePath]
splitPath = error "todo"

joinPath :: [FilePath] -> FilePath
joinPath = error "todo"

splitDirectories :: FilePath -> [FilePath]
splitDirectories = error "todo"

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive = error "todo"

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = error "todo"

takeDrive :: FilePath -> FilePath
takeDrive = error "todo"

hasDrive :: FilePath -> Bool
hasDrive = error "todo"

dropDrive :: FilePath -> FilePath
dropDrive = error "todo"

isDrive :: FilePath -> Bool
isDrive = error "todo"

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator = error "todo"

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator = error "todo"

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator = error "todo"

normalise :: FilePath -> FilePath
normalise = error "todo"

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath = error "todo"

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative = error "todo"

isRelative :: FilePath -> Bool
isRelative = error "todo"

isAbsolute :: FilePath -> Bool
isAbsolute = error "todo"

isValid :: FilePath -> Bool
isValid = error "todo"

makeValid :: FilePath -> FilePath
makeValid = error "todo"
