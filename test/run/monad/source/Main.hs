module Main where

liftM :: (Monad f) => (a -> b) -> f a -> f b
liftM f a = do
  a <- a
  return (f a)

ap :: (Monad f) => f (a -> b) -> f a -> f b
ap f a = do
  f <- f
  a <- a
  return (f a)

data Except a = Abort String | Valid a

instance Functor (Except) where
  fmap = liftM

instance Applicative (Except) where
  (<*>) = ap
  pure = Valid

instance Monad (Except) where
  Abort message >>= _ = Abort message
  Valid a >>= f = f a
  Abort message >> _ = Abort message
  Valid _ >> m = m
  return = pure

valid :: String
valid = case Valid (\x -> x) <*> Valid "abc" of
  Abort message -> 'a' : message
  Valid message -> 'v' : message

abort :: String
abort = case Abort "def" <*> Valid "abc" of
  Abort message -> 'a' : message
  Valid message -> 'v' : message

combine :: String -> String -> String
combine [] ys = ys
combine (x : xs) ys = x : combine xs ys

main :: IO ()
main = putStrLn (combine valid abort)
