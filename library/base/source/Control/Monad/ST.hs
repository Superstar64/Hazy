module Control.Monad.ST
  ( ST,
    runST,
    fixST,
    RealWorld,
    stToIO,
  )
where

import Hazy (ST, placeholder, runST)

fixST :: (a -> ST s a) -> ST s a
fixST = placeholder

data RealWorld

stToIO :: ST RealWorld a -> IO a
stToIO = placeholder
