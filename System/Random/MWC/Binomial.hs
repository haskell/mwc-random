import           System.Random.Stateful
import           System.Random.MWC
import           System.Random.MWC.Distributions
import           Control.Monad.Reader
import           Numeric.SpecFunctions (logFactorial)
import           System.Random.MWC.CondensedTable


inv :: StatefulGen g m => Int -> Double -> g -> m Int
inv n p gen = do
  let q = 1 - p
      s = p / q
      a = fromIntegral (n + 1) * s
      r = q^n
      f (rPrev, uPrev, xPrev) = (rNew, uNew, xNew)
        where
          uNew = uPrev - rPrev
          xNew = xPrev + 1
          rNew = rPrev * ((a / fromIntegral xNew) - s)
  u <- uniformDoublePositive01M gen
  let (_, _, x) = until (\(r, u, _) -> u <= r) f (r, u, 0) in return x

inv'' :: StatefulGen g m => Int -> Int -> Double -> g -> m [Int]
inv'' m n p gen = do
  let q = 1 - p
      s = p / q
      a = fromIntegral (n + 1) * s
      r = q^n
      f (rPrev, uPrev, xPrev) = (rNew, uNew, xNew)
        where
          uNew = uPrev - rPrev
          xNew = xPrev + 1
          rNew = rPrev * ((a / fromIntegral xNew) - s)
  replicateM m $ do u <- uniformDoublePositive01M gen
                    let (_, _, x) = until (\(r, u, _) -> u <= r) f (r, u, 0) in return x

inv' :: StatefulGen g m => g -> ReaderT (Double, Double, Double) m Int
inv' g = do
  (s, a, r) <- ask
  let f (rPrev, uPrev, xPrev) = (rNew, uNew, xNew)
        where
          uNew = uPrev - rPrev
          xNew = xPrev + 1
          rNew = rPrev * ((a / fromIntegral xNew) - s)
  u <- lift $ uniformDoublePositive01M g
  let (_, _, x) = until (\(r, u, _) -> u <= r) f (r, u, 0) in return x

ber :: StatefulGen g m => Int -> Double -> g -> m Int
ber n p g = fmap sum $ fmap (fmap fromEnum) $ replicateM n $ bernoulli p g

nSamples = 100000

testInv :: StatefulGen g m => g -> m Double
testInv g = do
  ss <- replicateM nSamples $ inv 1400 0.4 g
  return $ fromIntegral (sum ss) / fromIntegral nSamples

testTPE :: StatefulGen g m => g -> m Double
testTPE g = do
  ss <- replicateM nSamples $ binomial 1400 0.4 g
  return $ fromIntegral (sum ss) / fromIntegral nSamples

testInv'' :: StatefulGen g m => g -> m Double
testInv'' g = do
  ss <- inv'' nSamples 1400 0.4 g
  return $ fromIntegral (sum ss) / fromIntegral nSamples

testInv' :: StatefulGen g m => g -> ReaderT (Double, Double, Double) m Double
testInv' g = do
  ss <- replicateM nSamples $ inv' g
  return $ fromIntegral (sum ss) / fromIntegral nSamples

testTab g = do
  ss <- replicateM nSamples $ genFromTable (tableBinomial 1400 0.4) g
  return $ fromIntegral (sum ss) / fromIntegral nSamples

testBer :: StatefulGen g m => g -> m Double
testBer g = do
  tt <- replicateM nSamples $ ber 1400 0.4 g
  return $ fromIntegral (sum tt) / fromIntegral nSamples

mainB :: IO ()
mainB = do
  monadicGen <- create
  x <- runReaderT (ReaderT testBer) monadicGen
  print x

mainI :: IO ()
mainI = do
  monadicGen <- create
  x <- runReaderT (ReaderT testInv) monadicGen
  print x

mainTPE :: IO ()
mainTPE = do
  monadicGen <- create
  x <- runReaderT (ReaderT testTPE) monadicGen
  print x

mainII :: IO ()
mainII = do
  monadicGen <- create
  x <- runReaderT (ReaderT testInv'') monadicGen
  print x

mainMI :: IO ()
mainMI = do
  let n = 1400
      p = 0.4
      q = 1 - p
      s = p / q
      a = fromIntegral (n + 1) * s
      r = q^n
  g <- create
  x <- runReaderT (ReaderT (runReaderT (testInv' g))) (s, a, r)
  print x

mainTab :: IO ()
mainTab = do
  g <- create
  x <- testTab g
  print x

-- Threshold for preferring the BINV algorithm / inverse cdf
-- logic. The paper suggests 10, Ranlib uses 30, R uses 30, Rust uses
-- 10 and GSL uses 14.
bInvThreshold :: Double
bInvThreshold = 10

bInvMaxX :: Int
bInvMaxX = 110
