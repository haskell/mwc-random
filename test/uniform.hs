-- Tests for testing uniformity of distributions
--
-- Require statistics >= 0.7

{-# LANGUAGE BangPatterns #-}

import Data.Word
import Data.Int
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import Statistics.Distribution
import Statistics.Distribution.ChiSquared

import System.Random.MWC

import Text.Printf


-- Fill vector with number of occurences of random number in range.
-- Uses uniformR for generation of random numbers
fill :: (Variate a, U.Unbox a, Integral a) => 
        Int                     -- Expected number of items in each bin
     -> (a,a)                   -- Range for values
     -> GenIO                   -- Generator
     -> IO (U.Vector Int)
fill n rng@(x1,x2) g = do
  let l = fromIntegral x2 - fromIntegral x1 + 1
  v <- M.newWith l 0
  let loop k | k == n*l  = return ()
             | otherwise = do x <- fromIntegral `fmap` uniformR rng g
                              M.write v x . (+1) =<< M.read v x
                              loop (k+1)
  loop 0
  G.unsafeFreeze v

-- Calculate χ² statistics for vector of number occurences for
-- hypotheshys that each bin has equal probability
chi2uniform :: U.Vector Int -> Double
chi2uniform v = (U.sum $ U.map (sqr . subtract μ . fromIntegral) v) / μ 
  where
    n   = U.length v
    tot = U.sum v
    μ   = fromIntegral tot / fromIntegral n
    sqr x = x * x

-- Perform χ² on vector of number of occurences
checkChi2 :: Double             -- Desired significance level
          -> U.Vector Int       -- Vector of values
          -> IO ()
checkChi2 p v = do
  let χ2   = chi2uniform v      -- Observed χ²
      ndf  = U.length v - 1     -- N degrees of freedom
      d    = chiSquared ndf     -- Theoretical distribution
      pLow = cumulative d χ2
      pHi  = 1 - pLow
  
  putStrLn $ if pLow > p && (1-pLow) > p then "OK" else "* FAILED *"
  printf "  significance = %.3f\n"   p
  printf "  χ²/ndf = %.3f\n"        (χ2 / fromIntegral ndf)
  printf "  p(χ² < observed) = %.3g\n" pLow
  printf "  p(χ² > observed) = %.3g\n" pHi


main :: IO ()
main = do
  putStrLn "(0,255) Word8"
  v1 <- withSystemRandom $ fill 10000 (0,255 :: Word8)
  checkChi2 0.05 v1
  checkChi2 0.01 v1
  putStrLn ""
  ----------------------------------------
  putStrLn "(0,254) Word8"
  v1 <- withSystemRandom $ fill 10000 (0,254 :: Word8)
  checkChi2 0.05 v1
  checkChi2 0.01 v1
  putStrLn ""
  ----------------------------------------
  putStrLn "(0,10) Word8"
  v1 <- withSystemRandom $ fill 10000 (0,10 :: Word8)
  checkChi2 0.05 v1
  checkChi2 0.01 v1
  putStrLn ""
