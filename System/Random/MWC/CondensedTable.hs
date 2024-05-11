{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : System.Random.MWC.CondensedTable
-- Copyright : (c) 2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Table-driven generation of random variates.  This approach can
-- generate random variates in /O(1)/ time for the supported
-- distributions, at a modest cost in initialization time.
module System.Random.MWC.CondensedTable (
    -- * Condensed tables
    CondensedTable
  , CondensedTableV
  , CondensedTableU
  , genFromTable
    -- * Constructors for tables
  , tableFromProbabilities
  , tableFromWeights
  , tableFromIntWeights
    -- ** Disrete distributions
  , tablePoisson
  , tableBinomial
    -- * References
    -- $references
  ) where

import Control.Arrow           (second,(***))

import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Vector.Generic         as G
import           Data.Vector.Generic           ((++))
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                 as V
import Data.Vector.Generic (Vector)
import Numeric.SpecFunctions (logFactorial)
import System.Random.Stateful

import Prelude hiding ((++))



-- | A lookup table for arbitrary discrete distributions. It allows
-- the generation of random variates in /O(1)/. Note that probability
-- is quantized in units of @1/2^32@, and all distributions with
-- infinite support (e.g. Poisson) should be truncated.
data CondensedTable v a =
  CondensedTable
  {-# UNPACK #-} !Word64 !(v a) -- Lookup limit and first table
  {-# UNPACK #-} !Word64 !(v a) -- Second table
  {-# UNPACK #-} !Word64 !(v a) -- Third table
  !(v a)                        -- Last table

-- Implementation note. We have to store lookup limit in Word64 since
-- we need to accomodate two cases. First is when we have no values in
-- lookup table, second is when all elements are there
--
-- Both are pretty easy to realize. For first one probability of every
-- outcome should be less then 1/256, latter arise when probabilities
-- of two outcomes are [0.5,0.5]

-- | A 'CondensedTable' that uses unboxed vectors.
type CondensedTableU = CondensedTable U.Vector

-- | A 'CondensedTable' that uses boxed vectors, and is able to hold
-- any type of element.
type CondensedTableV = CondensedTable V.Vector



-- | Generate a random value using a condensed table.
genFromTable :: (StatefulGen g m, Vector v a) => CondensedTable v a -> g -> m a
{-# INLINE genFromTable #-}
genFromTable table gen = do
  w <- uniformM gen
  return $! lookupTable table $ fromIntegral (w :: Word32)

lookupTable :: Vector v a => CondensedTable v a -> Word64 -> a
{-# INLINE lookupTable #-}
lookupTable (CondensedTable na aa nb bb nc cc dd) i
  | i < na    = aa `at` ( i       `shiftR` 24)
  | i < nb    = bb `at` ((i - na) `shiftR` 16)
  | i < nc    = cc `at` ((i - nb) `shiftR` 8 )
  | otherwise = dd `at` ( i - nc)
  where
    at arr j = G.unsafeIndex arr (fromIntegral j)


----------------------------------------------------------------
-- Table generation
----------------------------------------------------------------

-- | Generate a condensed lookup table from a list of outcomes with
-- given probabilities. The vector should be non-empty and the
-- probabilities should be non-negative and sum to 1. If this is not
-- the case, this algorithm will construct a table for some
-- distribution that may bear no resemblance to what you intended.
tableFromProbabilities
    :: (Vector v (a,Word32), Vector v (a,Double), Vector v a, Vector v Word32)
       => v (a, Double) -> CondensedTable v a
{-# INLINE tableFromProbabilities #-}
tableFromProbabilities v
  | G.null tbl = pkgError "tableFromProbabilities" "empty vector of outcomes"
  | otherwise  = tableFromIntWeights $ G.map (second $ toWeight . (* mlt)) tbl
  where
    -- 2^32. N.B. This number is exatly representable.
    mlt = 4.294967296e9
    -- Drop non-positive probabilities
    tbl = G.filter ((> 0) . snd) v
    -- Convert Double weight to Word32 and avoid overflow at the same
    -- time. It's especially dangerous if one probability is
    -- approximately 1 and others are 0.
    toWeight w | w > mlt - 1 = 2^(32::Int) - 1
               | otherwise   = round w


-- | Same as 'tableFromProbabilities' but treats number as weights not
-- probilities. Non-positive weights are discarded, and those
-- remaining are normalized to 1.
tableFromWeights
    :: (Vector v (a,Word32), Vector v (a,Double), Vector v a, Vector v Word32)
       => v (a, Double) -> CondensedTable v a
{-# INLINE tableFromWeights #-}
tableFromWeights = tableFromProbabilities . normalize . G.filter ((> 0) . snd)
  where
    normalize v
      | G.null v  = pkgError "tableFromWeights" "no positive weights"
      | otherwise = G.map (second (/ s)) v
      where
        -- Explicit fold is to avoid 'Vector v Double' constraint
        s = G.foldl' (flip $ (+) . snd) 0 v


-- | Generate a condensed lookup table from integer weights. Weights
-- should sum to @2^32@ at least approximately. This function will
-- correct small deviations from @2^32@ such as arising from rounding
-- errors. But for large deviations it's likely to product incorrect
-- result with terrible performance.
tableFromIntWeights :: (Vector v (a,Word32), Vector v a, Vector v Word32)
                    => v (a, Word32)
                    -> CondensedTable v a
{-# INLINE tableFromIntWeights #-}
tableFromIntWeights v
  | n == 0    = pkgError "tableFromIntWeights" "empty table"
    -- Single element tables should be treated separately. Otherwise
    -- they will confuse correctWeights
  | n == 1    = let m = 2^(32::Int) - 1 -- Works for both Word32 & Word64
                in CondensedTable
                   m (G.replicate 256 $ fst $ G.head tbl)
                   m  G.empty
                   m  G.empty
                      G.empty
  | otherwise = CondensedTable
                na aa
                nb bb
                nc cc
                   dd
  where
    -- We must filter out zero-probability outcomes because they may
    -- confuse weight correction algorithm
    tbl   = G.filter ((/=0) . snd) v
    n     = G.length tbl
    -- Corrected table
    table = uncurry G.zip $ id *** correctWeights $ G.unzip tbl
    -- Make condensed table
    mkTable  d =
      G.concatMap (\(x,w) -> G.replicate (fromIntegral $ digit d w) x) table
    len = fromIntegral . G.length
    -- Tables
    aa = mkTable 0
    bb = mkTable 1
    cc = mkTable 2
    dd = mkTable 3
    -- Offsets
    na =       len aa `shiftL` 24
    nb = na + (len bb `shiftL` 16)
    nc = nb + (len cc `shiftL` 8)


-- Calculate N'th digit base 256
digit :: Int -> Word32 -> Word32
digit 0 x =  x `shiftR` 24
digit 1 x = (x `shiftR` 16) .&. 0xff
digit 2 x = (x `shiftR` 8 ) .&. 0xff
digit 3 x =  x .&. 0xff
digit _ _ = pkgError "digit" "the impossible happened!?"
{-# INLINE digit #-}

-- Correct integer weights so they sum up to 2^32. Array of weight
-- should contain at least 2 elements.
correctWeights :: G.Vector v Word32 => v Word32 -> v Word32
{-# INLINE correctWeights #-}
correctWeights v = G.create $ do
  let
    -- Sum of weights
    s = G.foldl' (flip $ (+) . fromIntegral) 0 v :: Int64
    -- Array size
    n = G.length v
  arr <- G.thaw v
  -- On first pass over array adjust only entries which are larger
  -- than `lim'. On second and subsequent passes `lim' is set to 1.
  --
  -- It's possibly to make this algorithm loop endlessly if all
  -- weights are 1 or 0.
  let loop lim i delta
        | delta == 0 = return ()
        | i >= n     = loop 1 0 delta
        | otherwise  = do
            w <- M.read arr i
            case () of
              _| w < lim   -> loop lim (i+1) delta
               | delta < 0 -> M.write arr i (w + 1) >> loop lim (i+1) (delta + 1)
               | otherwise -> M.write arr i (w - 1) >> loop lim (i+1) (delta - 1)
  loop 255 0 (s - 2^(32::Int))
  return arr


-- | Create a lookup table for the Poisson distribution. Note that
-- table construction may have significant cost. For λ < 100 it
-- takes as much time to build table as generation of 1000-30000
-- variates.
tablePoisson :: Double -> CondensedTableU Int
tablePoisson = tableFromProbabilities . make
  where
    make lam
      | lam < 0    = pkgError "tablePoisson" "negative lambda"
      | lam < 22.8 = U.unfoldr unfoldForward (exp (-lam), 0)
      | otherwise  = U.unfoldr unfoldForward (pMax, nMax)
                  ++ U.tail (U.unfoldr unfoldBackward (pMax, nMax))
      where
        -- Number with highest probability and its probability
        --
        -- FIXME: this is not ideal precision-wise. Check if code
        --        from statistics gives better precision.
        nMax = floor lam :: Int
        pMax = exp $ fromIntegral nMax * log lam - lam - logFactorial nMax
        -- Build probability list
        unfoldForward (p,i)
          | p < minP  = Nothing
          | otherwise = Just ( (i,p)
                             , (p * lam / fromIntegral (i+1), i+1)
                             )
        -- Go down
        unfoldBackward (p,i)
          | p < minP  = Nothing
          | otherwise = Just ( (i,p)
                             , (p / lam * fromIntegral i, i-1)
                             )
    -- Minimal representable probability for condensed tables
    minP = 1.1641532182693481e-10 -- 2**(-33)

-- | Create a lookup table for the binomial distribution.
tableBinomial :: Int            -- ^ Number of tries
              -> Double         -- ^ Probability of success
              -> CondensedTableU Int
tableBinomial n p = tableFromProbabilities makeBinom
  where 
  makeBinom
    | n <= 0         = pkgError "tableBinomial" "non-positive number of tries"
    | p == 0         = U.singleton (0,1)
    | p == 1         = U.singleton (n,1)
    | p > 0 && p < 1 = U.unfoldrN (n + 1) unfolder ((1-p)^n, 0)
    | otherwise      = pkgError "tableBinomial" "probability is out of range"
    where
      h = p / (1 - p)
      unfolder (t,i) = Just ( (i,t)
                            , (t * (fromIntegral $ n + 1 - i1) * h / fromIntegral i1, i1) )
        where i1 = i + 1

pkgError :: String -> String -> a
pkgError func err =
    error . concat $ ["System.Random.MWC.CondensedTable.", func, ": ", err]

-- $references
--
-- * Wang, J.; Tsang, W. W.; G. Marsaglia (2004), Fast Generation of
--   Discrete Random Variables, /Journal of Statistical Software,
--   American Statistical Association/, vol. 11(i03).
--   <http://ideas.repec.org/a/jss/jstsof/11i03.html>
