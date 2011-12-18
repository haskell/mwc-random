-- Tests for testing uniformity of distributions
module Uniform (
  tests
  ) where

import Data.Word
import Data.Int
import Data.Typeable
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Statistics.Test.ChiSquared

import System.Random.MWC

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Text.Printf


tests :: GenIO -> Test
tests g = testGroup "Uniformity" 
  [ testUniformity g (0,255 :: Word8 )
  , testUniformity g (0,254 :: Word8 )
  , testUniformity g (0,10  :: Word8 )
  ]


----------------------------------------------------------------

-- Test that uniformR is uniform using Chi-2 test
testUniformity :: (Variate a, U.Unbox a, Integral a, Typeable a, Show a) => GenIO -> (a,a) -> Test
testUniformity g (a,b) = 
  testCase ("uniformity " ++ show (typeOf a) ++ " " ++ show (a,b)) $ do
    let n = 10000
    vec <- fill n (a,b) g
    case chi2test 0.05 0 $ U.map (\x -> (x, fromIntegral n)) vec of
      Significant    -> assertFailure "Not uniform!"
      NotSignificant -> return ()


-- Fill vector with number of occurences of random number in range.
-- Uses uniformR for generation of random numbers
fill :: (Variate a, U.Unbox a, Integral a) => 
        Int                     -- Expected number of items in each bin
     -> (a,a)                   -- Range for values
     -> GenIO                   -- Generator
     -> IO (U.Vector Int)
fill n rng@(x1,x2) g = do
  let l = fromIntegral x2 - fromIntegral x1 + 1
  v <- M.replicate l 0
  let loop k | k == n*l  = return ()
             | otherwise = do x <- fromIntegral `fmap` uniformR rng g
                              M.write v x . (+1) =<< M.read v x
                              loop (k+1)
  loop 0
  G.unsafeFreeze v
