module Main where
import Deque as D
import Covariance
import Data.Time.Clock
import Data.IORef
import Control.Monad
import System.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L
import Control.DeepSeq
import Control.Exception
import Statistics.Sample

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

-- Generates a lazy list of windows
windows :: Int -> U.Vector Double -> U.Vector Double -> [U.Vector (Double, Double)]
windows k xs ys = let
  zs = pair xs ys
  in flip L.unfoldr zs $ \vec -> if U.length vec >= k
    then Just (U.take k vec, U.drop 1 vec)
    else Nothing

queues :: Int -> U.Vector Double -> U.Vector Double -> [Deque Covariance]
queues k xs ys = let
  zs = uncurry mkCovariance <$> U.toList (pair xs ys)
  pushPop x d = case D.pop (D.push x d) of
    Nothing -> error "Impossible situation: push resulted in empty queue"
    Just d' -> snd d'
  go d x = if D.size d == k
    then pushPop x d
    else push x d
  in dropWhile (\d -> D.size d < k) $ L.scanl' go cov zs

time :: String -> IO a -> IO a
time msg action = do
  t0 <- getCurrentTime
  r  <- action
  t1 <- getCurrentTime
  putStrLn $ msg ++ " took " ++ show (t1 `diffUTCTime` t0)
  return r

covariance' :: Deque Covariance -> Double
covariance' = maybe (0/0) getCovariance . runFold

-- simple test suite
main :: IO ()
main = do
  -- Find the rolling covariance of two randomly generated, million element vectors
  xs <- evaluate . force =<< U.fromList <$> replicateM 1000000 randomIO
  ys <- evaluate . force =<< U.fromList <$> replicateM 1000000 randomIO
  time "Naive 100"    $ print $ sum $ covariance  <$> windows 100 xs ys
  time "Window 100"   $ print $ sum $ covariance' <$> queues  100 xs ys
  time "Naive 1000"   $ print $ sum $ covariance  <$> windows 1000 xs ys
  time "Window 1000"  $ print $ sum $ covariance' <$> queues  1000 xs ys
  time "Naive 10000"  $ print $ sum $ covariance  <$> windows 10000 xs ys
  time "Window 10000" $ print $ sum $ covariance' <$> queues  10000 xs ys

