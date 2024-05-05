import           System.Random.Stateful
import           System.Random.MWC
import           System.Random.MWC.Distributions {- FIXME -} hiding (binomial)
import           Control.Monad.Reader
import           Numeric.SpecFunctions (logFactorial)

-- | Random variate generator for Binomial distribution
--
-- The probability of getting exactly k successes in n trials is
-- given by the probability mass function:
--
-- \[
-- f(k;n,p) = \Pr(X = k) = \binom n k  p^k(1-p)^{n-k}
-- \]
binomial :: forall g m . StatefulGen g m
         => Int               -- ^ Number of trials
         -> Double            -- ^ Probability of success (returning True)
         -> g                 -- ^ Generator
         -> m Int
binomial n p g =
  let q    = 1 - p
      np   = fromIntegral n * p
      ffm  = np + p
      bigM = floor ffm
      -- Half integer mean (tip of triangle)
      xm   = fromIntegral bigM + 0.5
      npq  = np * q

      -- p1: the distance to the left and right edges of the triangle
      -- region below the target distribution; since height=1, also:
      -- area of region (half base * height)
      p1 = fromIntegral (floor (2.195 * sqrt npq - 4.6 * q)) + 0.5
      -- Left edge of triangle
      xl = xm - p1
      -- Right edge of triangle
      xr = xm + p1
      c  = 0.134 + 20.5 / (15.3 + fromIntegral bigM)
      -- p1 + area of parallelogram region
      p2 = p1 * (1.0 + c + c)
      al = (ffm - xl) / (ffm - xl * p)
      lambdaL = al * (1.0 + 0.5 * al)
      ar = (xr - ffm) / (xr * q)
      lambdaR = ar * (1.0 + 0.5 * ar)

      -- p2 + area of left tail
      p3 = p2 + c / lambdaL
      -- p3 + area of right tail
      p4 = p3 + c / lambdaR


  -- Acceptance / rejection comparison
      step5 :: Int -> Double -> m Int
      step5 ix v = if var <= accept
                   then if p > 0
                        then return ix
                        else return $ n - ix
                   else hh
                    where
                      var = log v
                      accept = logFactorial bigM + logFactorial (n - bigM) -
                               logFactorial ix - logFactorial (n - ix) +
                               fromIntegral (ix - bigM) * log (p / q)

      h :: Double -> Double -> m Int
      h u v | -- Triangular region
              u <= p1 = return $ floor $ xm - p1 * v + u

              -- Parallelogram region
            | u <= p2 = do let x = xl + (u - p1) / c
                               w = v * c + 1.0 - abs (x - xm) / p1
                           if w > 1 || w <= 0
                            then hh
                            else do let ix = floor x
                                    step5 ix w

              -- Left tail
            | u <= p3 = do let ix = floor $ xl + log v / lambdaL
                           if ix < 0
                             then hh
                             else do let w = v * (u - p2) * lambdaL
                                     step5 ix w

              -- Right tail
            | otherwise = do let ix = floor $ xr - log v / lambdaL
                             if ix > 0 && ix > n
                               then hh
                               else do let w = v * (u - p3) * lambdaR
                                       step5 ix w

      hh = do
        u <- uniformRM (0.0, p4) g
        v <- uniformDoublePositive01M g
        h u v

  in hh

-- binomial n p g =
--   if p == 0.0
--   then return 0
--   else if p == 1.0
--        then return n
--        else do
--     let q = 1 - p
--     if fromIntegral n * p < bInvThreshold
--       then do
--       let s = p / q
--       let a = fromIntegral (n + 1) * s
--       bar n p g
--       else baz n p g

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

nSamples = 1000000

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


-- Threshold for preferring the BINV algorithm / inverse cdf
-- logic. The paper suggests 10, Ranlib uses 30, R uses 30, Rust uses
-- 10 and GSL uses 14.
bInvThreshold :: Double
bInvThreshold = 10

bInvMaxX :: Int
bInvMaxX = 110
