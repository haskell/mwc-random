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
      undefined
      else undefined

-- FIXME: The paper contains mysterious (to me at least)
-- constants. The Rust author seems to understand these and makes
-- comments such as
--
--  * radius of triangle region, since height=1 also area of region
--
-- The Rust author also comments
--
--  * It is possible for BINV to get stuck, so we break if x >
-- BINV_MAX_X and try again.
--
-- * It would be safer to set BINV_MAX_X to self.n, but it is
-- extremely unlikely to be relevant.
--
-- * When n*p < 10, so is n*p*q which is the variance, so a result >
-- 110 would be 100 / sqrt(10) = 31 standard deviations away.


baz :: StatefulGen g m => Int -> Double -> g -> m Int
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
      p2 = p1 * (1 + 2 * c)
      lambda a = a * (1 + 0.5 * a)
      lambdaL = lambda ((fm - xl) / (fm - xl * p))
      lambdaR = lambda ((xr - fm) / (xr * q))
      -- FIXME: p1 + area of left tail
      p3 = p2 + c / lambdaL
      -- p1 + area of right tail
      p4 = p3 + c / lambdaR
  return undefined

bar :: StatefulGen g m => Int -> Double -> g -> m Int
bar n p gen = do
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

foo :: StatefulGen g m => Int -> Double -> g -> m Int
foo n p g = fmap sum $ fmap (fmap fromEnum) $ replicateM n $ bernoulli p g

nSamples = 100000

test :: StatefulGen g m => g -> m (Double, Double)
test g = do
  ss <- replicateM nSamples $ bar 20 0.4 g
  tt <- replicateM nSamples $ foo 20 0.4 g
  return $ ((fromIntegral (sum ss) / fromIntegral nSamples), (fromIntegral (sum tt) / fromIntegral nSamples))

main :: IO ()
main = do
  monadicGen <- create
  x <- runReaderT (ReaderT test) monadicGen
  print x

  -- if uPrev > rPrev

          -- if xPrev > bInvMaxX
          -- then foo s a undefined


-- Threshold for preferring the BINV algorithm / inverse cdf
-- logic. The paper suggests 10, Ranlib uses 30, R uses 30, Rust uses
-- 10 and GSL uses 14.
bInvThreshold :: Double
bInvThreshold = 10

bInvMaxX :: Int
bInvMaxX = 110
