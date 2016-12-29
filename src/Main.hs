module Main where
import Queue as Q
import qualified Covariance as C
import Data.Time.Clock
import Data.IORef
import Control.Monad
import System.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L
import Control.DeepSeq
import Control.Exception
import Statistics.Sample as S

summer :: Queue Int
summer = queue (+)

cov :: Queue C.Covariance
cov = queue C.updateCovariance

cov2 :: Queue Covariance2
cov2 = queue appCov2

pushTrace :: Show a => a -> IORef (Queue a) -> IO ()
pushTrace a xs = do
  modifyIORef xs (push a)
  xs' <- readIORef xs
  print $ runFold xs'

data Covariance2 = Cov2
  { sumx  :: {-#UNPACK#-} !Double
  , sumy  :: {-#UNPACK#-} !Double
  , cov0  :: {-#UNPACK#-} !Double -- partial covariance. basically cov xs ys * (len - 1),
                    -- i.e. just the sum without denominator
  , len   :: {-#UNPACK#-} !Int
  }

mkcov2 :: Double -> Double -> Covariance2
mkcov2 x y = Cov2
  { sumx = x
  , sumy = y
  , cov0 = 0 -- (x - (mean xs)) * (y - (mean ys))
  , len  = 1
  }

getCov2 :: Covariance2 -> Double
-- TODO investigate why it's len and not (len - 1)
-- (comparing against another library is len
getCov2 (Cov2 _ _ part len) = part / fromIntegral len
-- sum (zipWith (\x y -> (x - x0) * (y - y0)) xs ys) / (n - 1)

-- sum (zipWith (\x y -> (x - x0) * (x - x0)) xs xs) / (n - 1)
-- sum (map     (\x   -> (x - x1)^2) xs)             / n

appCov2 :: Covariance2 -> Covariance2 -> Covariance2
appCov2
  (Cov2 sumx0 sumy0 part0 len0_)
  (Cov2 sumx1 sumy1 part1 len1_)
  = Cov2
  { sumx = sumx0 + sumx1
  , sumy = sumy0 + sumy1
  , cov0 = part'
  , len  = len0_ + len1_
  }
  where
   part' = cov0 + cov1
     where
       len0 = fromIntegral len0_
       len1 = fromIntegral len1_
       -- | Correct the partial covariance with new mean
       meanx0 = sumx0 / len0
       meany0 = sumy0 / len0
       meanx1 = sumx1 / len1
       meany1 = sumy1 / len1
       meanx' = (sumx0 + sumx1) / (len0 + len1)
       meany' = (sumy0 + sumy1) / (len0 + len1)
       cov0 = part0 + len0 * (meanx' * meany' - meanx0 * meany0)
                    - sumx0 * (meany' - meany0)
                    - sumy0 * (meanx' - meanx0)
       cov1 = part1 + len1 * (meanx' * meany' - meanx1 * meany1)
                    - sumx1 * (meany' - meany1)
                    - sumy1 * (meanx' - meanx1)

popTrace :: Show a => IORef (Queue a) -> IO ()
popTrace xs = do
  modifyIORef xs (\xs -> maybe xs snd $ pop xs)
  xs' <- readIORef xs
  print $ runFold xs'

-- showCov = show . getCovariance

-- Generates a lazy list of windows
windows :: Int -> U.Vector Double -> U.Vector Double -> [U.Vector (Double, Double)]
windows k xs ys = let
  zs = pair xs ys
  in flip L.unfoldr zs $ \vec -> if U.length vec >= k
    then Just (U.take k vec, U.drop 1 vec)
    else Nothing

queues :: Int -> U.Vector Double -> U.Vector Double -> [Queue Covariance2]
queues k xs ys = let
  zs = uncurry mkcov2 <$> U.toList (pair xs ys)
  pushPop x d = case Q.pop (Q.push x d) of
    Nothing -> error "Impossible situation: push resulted in empty queue"
    Just d' -> snd d'
  go d x = if Q.size d == k
    then pushPop x d
    else push x d
  in dropWhile (\d -> Q.size d < k) $ L.scanl' go cov2 zs

time :: String -> IO a -> IO a
time msg action = do
  t0 <- getCurrentTime
  r  <- action
  t1 <- getCurrentTime
  putStrLn $ msg ++ " took " ++ show (t1 `diffUTCTime` t0)
  return r

covariance' :: Queue Covariance2 -> Double
covariance' = maybe (0/0) getCov2 . runFold

-- simple test suite
main :: IO ()
main = do
  -- Find the rolling covariance of two randomly generated, million element vectors
  xs <- evaluate . force =<< U.fromList <$> replicateM 100000 randomIO
  ys <- evaluate . force =<< U.fromList <$> replicateM 100000 randomIO
  time "Naive 10"    $ print $ sum $ covariance  <$> windows 10 xs ys
  time "Window 10"   $ print $ sum $ covariance' <$> queues  10 xs ys
  time "Naive 100"   $ print $ sum $ covariance  <$> windows 100 xs ys
  time "Window 100"  $ print $ sum $ covariance' <$> queues  100 xs ys
  time "Naive 1000"  $ print $ sum $ covariance  <$> windows 1000 xs ys
  time "Window 1000" $ print $ sum $ covariance' <$> queues  1000 xs ys

