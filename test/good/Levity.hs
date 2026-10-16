module Levity where

import Data.Kind (Lazy)

data Branch s
 = Normal
 | Poly ~!s Int 

normal :: Branch s
normal = Normal

data Both s1 s2 = Both ~!s1 Int ~!s2 Int

left :: Both Lazy s -> Int
left (Both x y) = x