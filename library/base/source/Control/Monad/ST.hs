module Control.Monad.ST
  ( ST,
    runST,
    fixST,
    RealWorld,
    stToIO,
  )
where

import Hazy (ST, runST)

fixST :: (a -> ST s a) -> ST s a
fixST = error "todo"

data RealWorld

stToIO :: ST RealWorld a -> IO a
stToIO = error "todo"
