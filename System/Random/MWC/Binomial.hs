import           System.Random.Stateful
import           System.Random.MWC
import           System.Random.MWC.Distributions
import           Control.Monad.Reader

-- | Random variate generator for Binomial distribution
--
-- The probability of getting exactly k successes in n trials is
-- given by the probability mass function:
--
-- \[
-- f(k;n,p) = \Pr(X = k) = \binom n k  p^k(1-p)^{n-k}
-- \]
binomial :: StatefulGen g m
         => Int               -- ^ Number of trials
         -> Double            -- ^ Probability of success (returning True)
         -> g                 -- ^ Generator
         -> m Int
binomial n p g = undefined

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

testInv'' :: StatefulGen g m => g -> m Double
testInv'' g = do
  ss <- inv'' nSamples 1400 0.4 g
  return $ fromIntegral (sum ss) / fromIntegral nSamples

testInv' :: StatefulGen g m => g -> ReaderT (Double, Double, Double) m Double
testInv' g = do
  ss <- replicateM nSamples $ inv' g
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
