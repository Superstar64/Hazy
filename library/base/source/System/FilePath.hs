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

import Hazy (placeholder)

pathSeparator :: Char
pathSeparator = placeholder

pathSeparators :: [Char]
pathSeparators = placeholder

isPathSeparator :: Char -> Bool
isPathSeparator = placeholder

searchPathSeparator :: Char
searchPathSeparator = placeholder

isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = placeholder

extSeparator :: Char
extSeparator = placeholder

isExtSeparator :: Char -> Bool
isExtSeparator = placeholder

splitSearchPath :: String -> [FilePath]
splitSearchPath = placeholder

getSearchPath :: IO [FilePath]
getSearchPath = placeholder

splitExtension :: FilePath -> (String, String)
splitExtension = placeholder

takeExtension :: FilePath -> String
takeExtension = placeholder

replaceExtension :: FilePath -> String -> FilePath
replaceExtension = placeholder

infixr 7 -<.>

(-<.>) :: FilePath -> String -> FilePath
(-<.>) = placeholder

dropExtension :: FilePath -> FilePath
dropExtension = placeholder

addExtension :: FilePath -> String -> FilePath
addExtension = placeholder

hasExtension :: FilePath -> Bool
hasExtension = placeholder

infixr 7 <.>

(<.>) :: FilePath -> String -> FilePath
(<.>) = placeholder

splitExtensions :: FilePath -> (FilePath, String)
splitExtensions = placeholder

dropExtensions :: FilePath -> FilePath
dropExtensions = placeholder

takeExtensions :: FilePath -> String
takeExtensions = placeholder

replaceExtensions :: FilePath -> String -> FilePath
replaceExtensions = placeholder

isExtensionOf :: String -> FilePath -> Bool
isExtensionOf = placeholder

stripExtension :: String -> FilePath -> Maybe FilePath
stripExtension = placeholder

splitFileName :: FilePath -> (String, String)
splitFileName = placeholder

takeFileName :: FilePath -> FilePath
takeFileName = placeholder

replaceFileName :: FilePath -> String -> FilePath
replaceFileName = placeholder

dropFileName :: FilePath -> FilePath
dropFileName = placeholder

takeBaseName :: FilePath -> String
takeBaseName = placeholder

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName = placeholder

takeDirectory :: FilePath -> FilePath
takeDirectory = placeholder

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory = placeholder

combine :: FilePath -> FilePath -> FilePath
combine = placeholder

infixr 5 </>

(</>) :: FilePath -> FilePath -> FilePath
(</>) = placeholder

splitPath :: FilePath -> [FilePath]
splitPath = placeholder

joinPath :: [FilePath] -> FilePath
joinPath = placeholder

splitDirectories :: FilePath -> [FilePath]
splitDirectories = placeholder

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive = placeholder

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = placeholder

takeDrive :: FilePath -> FilePath
takeDrive = placeholder

hasDrive :: FilePath -> Bool
hasDrive = placeholder

dropDrive :: FilePath -> FilePath
dropDrive = placeholder

isDrive :: FilePath -> Bool
isDrive = placeholder

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator = placeholder

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator = placeholder

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator = placeholder

normalise :: FilePath -> FilePath
normalise = placeholder

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath = placeholder

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative = placeholder

isRelative :: FilePath -> Bool
isRelative = placeholder

isAbsolute :: FilePath -> Bool
isAbsolute = placeholder

isValid :: FilePath -> Bool
isValid = placeholder

makeValid :: FilePath -> FilePath
makeValid = placeholder
