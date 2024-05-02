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
binomial n p g =
  if p == 0.0
  then return 0
  else if p == 1.0
       then return n
       else do
    let q = 1 - p
    if fromIntegral n * p < bInvThreshold
      then do
      let s = p / q
      let a = fromIntegral (n + 1) * s
      bar n p g
      else baz n p g

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

baz :: forall g m . StatefulGen g m => Int -> Double -> g -> m Int
baz n p g = do
  let q   = if p <= 0.5 then p else 1 - p
      np  = fromIntegral n * p
      npq = np * q
      fm  = np + p
      m   = floor fm
      p1 = fromIntegral (floor (2.195 * sqrt npq - 4.6 * q)) + 0.5
      -- FIXME: This comment I understand and will come back to
      -- Tip of triangle
      xm = fromIntegral m + 0.5
      -- Left edge of triangle
      xl = xm - p1
      -- Right edge of triangle
      xr = xm + p1
      -- FIXME: I am not sure I understand this
      -- p1 + area of parallelogram region
      c  = 0.134 + 20.5 / (15.3 + fromIntegral m)
      p2 = p1 * (1.0 + 2.0 * c)
      lambda a = a * (1.0 + 0.5 * a)
      lambdaL = lambda ((fm - xl) / (fm - xl * p))
      lambdaR = lambda ((xr - fm) / (xr * q))
      -- FIXME: p1 + area of left tail
      p3 = p2 + c / lambdaL
      -- p1 + area of right tail
      p4 = p3 + c / lambdaR
      f :: m Int
      f = do
        u <- uniformRM (0.0, p4) g
        v <- uniformDoublePositive01M g
        y <- if u <= p1
             then return $ floor $ xm - p1 * v + u
             else if u <= p2
                  then undefined
                  else undefined
        return undefined
                  -- then do let x = xl + (u - p1) / c
                  --             v = v * c + 1.0 - abs (x - xm) / p1
                  --         if v > 1
                  --           then f
                  --           else return $ floor x
                  -- else return undefined
  return undefined

-- Threshold for preferring the BINV algorithm / inverse cdf
-- logic. The paper suggests 10, Ranlib uses 30, R uses 30, Rust uses
-- 10 and GSL uses 14.
bInvThreshold :: Double
bInvThreshold = 10

bInvMaxX :: Int
bInvMaxX = 110