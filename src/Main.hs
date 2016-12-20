module Main where
import Deque
import Data.IORef

summer :: Deque Int
summer = deque (+)

pushTrace :: Show a => a -> IORef (Deque a) -> IO ()
pushTrace a xs = do
  modifyIORef xs (push a)
  xs' <- readIORef xs
  print $ runFold xs'

popTrace :: Show a => IORef (Deque a) -> IO ()
popTrace xs = do
  modifyIORef xs (\xs -> maybe xs snd $ pop xs)
  xs' <- readIORef xs
  print $ runFold xs'

-- simple test suite
main :: IO ()
main = do
  x <- newIORef summer
  pushTrace 1 x -- 1
  pushTrace 2 x -- 3
  pushTrace 3 x -- 6
  popTrace x    -- 5 [2,3]
  popTrace x    -- 3 [3]
  pushTrace 2 x -- 5 [3,2]
  pushTrace 3 x -- 8 [3,2,3]
  popTrace x    -- 5 [2,3]
  popTrace x    -- 3 [3]
  pushTrace 1 x -- 4 [3,1]
  popTrace x    -- 1 [1]
  popTrace x    -- Nothing

