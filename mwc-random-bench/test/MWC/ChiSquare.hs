{-# LANGUAGE RecordWildCards #-}
-- Chi square tests for random generators
module MWC.ChiSquare (
  tests
  ) where

import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.Word
import Data.List (find)
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as M
import qualified System.Random.MWC                as MWC
import qualified System.Random.MWC.Distributions  as MWC
import qualified System.Random.MWC.CondensedTable as MWC

import Statistics.Types
import Statistics.Test.ChiSquared
import Statistics.Distribution
import Statistics.Distribution.Poisson
import Statistics.Distribution.Binomial
import Statistics.Distribution.Geometric

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit



----------------------------------------------------------------

tests :: MWC.GenIO -> Test.Framework.Test
tests g = testGroup "Chi squared tests"
    -- Word8 tests
  [ uniformRTest (0,255 :: Word8) g
  , uniformRTest (0,254 :: Word8) g
  , uniformRTest (0,129 :: Word8) g
  , uniformRTest (0,126 :: Word8) g
  , uniformRTest (0,10  :: Word8) g
    -- * Tables
  , ctableTest   [1] g
  , ctableTest   [0.5,  0.5] g
  , ctableTest   [0.25, 0.25, 0.25, 0.25] g
  , ctableTest   [0.25, 0.5,  0.25] g
  , ctableTest   [1/3 , 1/3, 1/3] g
  , ctableTest   [0.1,  0.9] g
  , ctableTest   (replicate 10 0.1) g
    -- ** Poisson
  , poissonTest 0.2  g
  , poissonTest 1.32 g
  , poissonTest 6.8  g
  , poissonTest 100  g
    -- ** Binomial
  , binomialTest 4   0.5 g
  , binomialTest 10  0.1 g
  , binomialTest 10  0.6 g
  , binomialTest 10  0.8 g
  , binomialTest 100 0.3 g
    -- ** Geometric
  , geometricTest 0.1 g
  , geometricTest 0.5 g
  , geometricTest 0.9 g
  ]

----------------------------------------------------------------
-- | RNG and corresonding distribution
data Generator = Generator {
    generator    :: MWC.GenIO -> IO Int
  , probabilites :: U.Vector Double
  }

-- | Apply chi square test for a distribution
sampleTest :: String              -- ^ Name of test
           -> Generator           -- ^ Generator to test
           -> Int                 -- ^ N of events
           -> MWC.GenIO           -- ^ PRNG state
           -> Test.Framework.Test
sampleTest nm (Generator{..}) n g = testCase nm $ do
  let size = U.length $ probabilites
  h <- histogram (generator g) size n
  let w      = U.map (* fromIntegral n) probabilites
      Just t = chi2test 0 $ U.zip h w
  case isSignificant (mkPValue 0.01) t of
    Significant    -> assertFailure ("Significant: " ++ show t)
    NotSignificant -> return ()
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


-- | Test uniformR
uniformRTest :: (MWC.Variate a, Typeable a, Show a, Integral a) => (a,a) -> MWC.GenIO -> Test.Framework.Test
uniformRTest (a,b)
  = sampleTest ("uniformR: " ++ show (a,b) ++ " :: " ++ show (typeOf a)) gen (10^5)
  where
    n   = fromIntegral b - fromIntegral a + 1
    gen = Generator { generator    = \g -> fromIntegral . subtract a <$> MWC.uniformR (a,b) g
                    , probabilites = U.replicate n (1 / fromIntegral n)
                    }
{-# INLINE uniformRTest #-}

-- | Test for condensed tables
ctableTest :: [Double] -> MWC.GenIO -> Test.Framework.Test
ctableTest ps
  = sampleTest ("condensedTable: " ++ show ps) gen (10^4)
  where
    gen = Generator
          { generator    = MWC.genFromTable $ MWC.tableFromProbabilities $ U.fromList $ zip [0..] ps
          , probabilites = U.fromList ps
          }


-- | Test for condensed table for poissson distribution
poissonTest :: Double -> MWC.GenIO -> Test.Framework.Test
poissonTest lam
  = sampleTest ("poissonTest: " ++ show lam) gen (10^4)
  where
    pois      = poisson lam
    Just nMax = find (\n -> probability pois n < 2**(-33)) [floor lam ..]
    gen = Generator
          { generator    = MWC.genFromTable (MWC.tablePoisson lam)
          , probabilites = U.generate nMax (probability pois)
          }

-- | Test for condensed table for binomial distribution
binomialTest :: Int -> Double -> MWC.GenIO -> Test.Framework.Test
binomialTest n p
  = sampleTest ("binomialTest: " ++ show p ++ " " ++ show n) gen (10^4)
  where
    binom = binomial n p
    gen   = Generator
            { generator    = MWC.genFromTable (MWC.tableBinomial n p)
            , probabilites = U.generate (n+1) (probability binom)
            }

-- | Test for geometric distribution
geometricTest :: Double -> MWC.GenIO -> Test.Framework.Test
geometricTest gd
  = sampleTest ("geometricTest: " ++ show gd) gen (10^4)
  where
    n   = 1000
    gen = Generator
          { generator    = MWC.geometric1 gd
          , probabilites = U.generate (n+1) (probability $ geometric gd)
          }
