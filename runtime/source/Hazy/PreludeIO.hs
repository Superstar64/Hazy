module Hazy.PreludeIO where

import Hazy.Prelude

putStrLn :: String -> IO ()
putStrLn = putStrLnText . pack
