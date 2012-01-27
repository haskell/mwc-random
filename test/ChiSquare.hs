{-# LANGUAGE RecordWildCards #-}
-- Chi square tests for random generators
module ChiSquare ( 
  tests
  ) where

import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.Word
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified System.Random.MWC           as MWC

import Statistics.Test.ChiSquared

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

----------------------------------------------------------------

tests :: MWC.GenIO -> Test
tests g = testGroup "Chi squared tests" 
    -- Word8 tests
  [ uniformRTest (0,255 :: Word8) g
  , uniformRTest (0,254 :: Word8) g
  , uniformRTest (0,129 :: Word8) g
  , uniformRTest (0,126 :: Word8) g
  , uniformRTest (0,10  :: Word8) g
    -- * Tables
  ]

----------------------------------------------------------------
-- | RNG and corresonding distribution
data Generator = Generator {
    generator    :: MWC.GenIO -> IO Int
  , probabilites :: U.Vector Double
  }

-- | Apply chi square test for a distribution
sampleTest :: Generator           -- ^ Generator to test
           -> Int                 -- ^ N of events
           -> MWC.GenIO           -- ^ PRNG state
           -> IO TestResult
sampleTest (Generator{..}) n g = do
  let size = U.length $ probabilites
  h <- histogram (generator g) size n
  let w = U.map (* fromIntegral n) probabilites
  return $ chi2test 0.05 0 $ U.zip h w
{-# INLINE sampleTest #-}

  
-- | Fill histogram using supplied generator
histogram :: IO Int             -- ^ Rangom generator
          -> Int                -- ^ N of outcomes 
          -> Int                -- ^ N of events
          -> IO (U.Vector Int)
histogram gen size n = do
  arr <- M.replicate size 0
  replicateM_ n $ do i <- gen
                     when (i < size) $ M.write arr i . (+1) =<< M.read arr i
  U.unsafeFreeze arr
{-# INLINE histogram #-}


uniformRTest :: (MWC.Variate a, Typeable a, Show a, Integral a) => (a,a) -> MWC.GenIO -> Test
uniformRTest (a,b) g = 
  testCase ("uniformR: " ++ show (a,b) ++ " :: " ++ show (typeOf a)) $ do
    let n   = fromIntegral b - fromIntegral a + 1
        gen = Generator { generator    = \g -> fromIntegral . subtract a <$> MWC.uniformR (a,b) g
                        , probabilites = U.replicate n (1 / fromIntegral n)
                        }
    r <- sampleTest gen (10^5) g
    assertEqual "Significant!" NotSignificant r
{-# INLINE uniformRTest #-}