{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module System.Console.GetOpt where

getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt = error "todo"

getOpt' :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String], [String])
getOpt' = error "todo"

usageInfo :: String -> [OptDescr a] -> String
usageInfo = error "todo"

data ArgOrder a
  = RequireOrder
  | Permute
  | ReturnInOrder (String -> a)

data OptDescr a = Option [Char] [String] (ArgDescr a) String

data ArgDescr a
  = NoArg a
  | ReqArg (String -> a) String
  | OptArg (Maybe String -> a) String
