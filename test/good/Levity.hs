module Levity where

data Branch s
 = Normal
 | Poly ~!s Int 

normal :: Branch s
normal = Normal