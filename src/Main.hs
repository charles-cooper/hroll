module Main where
import Deque
import Covariance
import Data.IORef

summer :: Deque Int
summer = deque (+)

cov :: Deque Covariance
cov = deque updateCovariance

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

showCov = show . getCovariance

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

  y <- newIORef cov
  -- check against numpy
  pushTrace (mkCovariance 1 1) y     -- [1], [1]
  pushTrace (mkCovariance 1 1.5) y   -- [1,1], [1.5,1]
  pushTrace (mkCovariance 0.9 0.5) y -- [0.9,1,1], [0.5,1.5,1]
  pushTrace (mkCovariance 0.8 0.2) y -- [0.8,0.9,1,1], [0.2,0.5,1.5,1]
  popTrace y                         -- [0.8,0.9,1], [0.2,0.5,1.5]
  popTrace y                         -- [0.8,0.9], [0.2,0.5]
  pushTrace (mkCovariance 0.0 0.0) y -- [0.0,0.8,0.9], [0.0,0.2,0.5]
  popTrace y                         -- [0.0,0.8], [0.0,0.2]
  popTrace y                         -- [0.0], [0.0]
  popTrace y                         -- [], []

