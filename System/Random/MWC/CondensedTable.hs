{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : System.Random.MWC.CondesedTable
-- Copyright : (c) 2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
module System.Random.MWC.CondensedTable (
    -- * Condensed tables
    CondensedTable
  , genFromTable
    -- * Constructors for tables
  , tableFromProbabilities
  , tableFromWeights
  , tableFromIntWeights
  ) where

import Control.Arrow           (second,(***))
import Control.Monad.Primitive (PrimMonad(..))

import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import Data.Vector.Generic (Vector)

import System.Random.MWC



-- 4 tables base 256
data CondensedTable v a =
  CondensedTable
  {-# UNPACK #-} !Word64 !(v a) -- Lookup limit and first table
  {-# UNPACK #-} !Word64 !(v a) -- Second table
  {-# UNPACK #-} !Word64 !(v a) -- Third table
  !(v a)                        -- Last table

genFromTable :: (PrimMonad m, Vector v a) => CondensedTable v a -> Gen (PrimState m) -> m a
{-# INLINE genFromTable #-}
genFromTable table gen = do
  w <- uniform gen
  return $ lookupTable table $ fromIntegral (w :: Word32)

lookupTable :: Vector v a => CondensedTable v a -> Word64 -> a
{-# INLINE lookupTable #-}
lookupTable (CondensedTable na aa nb bb nc cc dd) i
  | i < na    = aa `at` ( i       `shiftR` 24)
  | i < nb    = bb `at` ((i - na) `shiftR` 16)
  | i < nc    = cc `at` ((i - nb) `shiftR` 8 )
  | otherwise = dd `at` ( i - nc)
  where
    at arr j = (G.!) arr (fromIntegral j)

----------------------------------------------------------------
-- Table generation
----------------------------------------------------------------

-- | Generate condensed lookup table from list of outcomes with given
--   probabilities. Vector should be non-empty and probabilites should
--   be non-negative and add up to 1. If this is not the case
--   algorithm will construct valid table for some distribution which
--   may bear no resemblance to intended.
tableFromProbabilities :: (Vector v (a,Word32), Vector v (a,Double), Vector v a, Vector v Word32)
                       => v (a, Double)
                       -> CondensedTable v a
tableFromProbabilities v
  | G.null v  = error "System.Random.MWC.CondesedTable.tableFromProbabilities: null vector of outcomes"
  | otherwise = tableFromIntWeights $ G.map (second $ round . (*(2^32))) v

-- | Some as 'tableFromProbabilities' but treats number as weights not
--   probilities. Nonpositive weights are discarded and remaining are
--   normalized to 1.
tableFromWeights :: (Vector v (a,Word32), Vector v (a,Double), Vector v a, Vector v Word32)
                 => v (a, Double)
                 -> CondensedTable v a
tableFromWeights = tableFromProbabilities . normalize . G.filter ((> 0) . snd)
  where
    normalize v
      | G.null v  = error "System.Random.MWC.CondesedTable.tableFromWeights: no positive weights"
      | otherwise = G.map (second (/ s)) v
      where
        -- Explicit fold is to avoid 'Vector v Double' constraint
        s = G.foldl' (flip $ (+) . snd) 0 v

-- | Generate condensed lookup table from integer weights. Weights
--   should add up to @2^32@. If they doesn't algorithm will alter
--   weights so they will. It should work reasonably well for rounding
--   error.
tableFromIntWeights :: (Vector v (a,Word32), Vector v a, Vector v Word32)
                    => v (a, Word32)
                    -> CondensedTable v a
tableFromIntWeights tbl
  | n == 0    = error "System.Random.MWC.CondesedTable.tableFromIntWeights: empty table"
    -- Single element tables should be treated sepately. Otherwise
    -- they will confuse correctWeights
  | n == 1    = let m = 2^32 - 1 -- Works for both Word32 & Word64
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
digit _ _ = error "mwc-random: digit, impossible happened"
{-# INLINE digit #-}

-- Correct integer weights so they sum up to 2^32. Array of weight
-- should contain at least 2 elements.
correctWeights :: G.Vector v Word32 => v Word32 -> v Word32
correctWeights v = G.create $ do
  let
    -- Sum of weights
    s = G.foldl' (flip $ (+) . fromIntegral) 0 v :: Int64
    -- Array size
    n = G.length v
  arr <- G.thaw v
  -- On first pass over array adjust only entries which are larger
  -- than `lim'. On second and consequent passes `lim' is set to 1
  --
  -- It's possibly to make this algorithm loop endlessly if all
  -- weights are 1 or 0
  let loop lim i delta
        | delta == 0 = return ()
        | i >= n     = loop 1 0 delta
        | otherwise  = do
            w <- M.read arr i
            case () of
              _| w < lim   -> loop lim (i+1) delta
               | delta < 0 -> M.write arr i (w + 1) >> loop lim (i+1) (delta + 1)
               | otherwise -> M.write arr i (w - 1) >> loop lim (i+1) (delta - 1)
  loop 255 0 (s - 2^32)
  return arr


-- $references
--
-- * Wang, J.; Tsang, W. W.; G. Marsaglia (2004), Fast Generation of
--   Discrete Random Variables, /Journal of Statistical Software,
--   American Statistical Association/, vol. 11(i03).
--   <http://ideas.repec.org/a/jss/jstsof/11i03.html>

