module System.Console.GetOpt where

import Hazy.Prelude (placeholder)

getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt = placeholder

getOpt' :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String], [String])
getOpt' = placeholder

usageInfo :: String -> [OptDescr a] -> String
usageInfo = placeholder

data ArgOrder a
  = RequireOrder
  | Permute
  | ReturnInOrder (String -> a)

data OptDescr a = Option [Char] [String] (ArgDescr a) String

data ArgDescr a
  = NoArg a
  | ReqArg (String -> a) String
  | OptArg (Maybe String -> a) String
