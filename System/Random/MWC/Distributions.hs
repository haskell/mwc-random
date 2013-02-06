{-# LANGUAGE BangPatterns #-}
-- |
-- Module    : System.Random.MWC.Distributions
-- Copyright : (c) 2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation for non-uniform distributions.

module System.Random.MWC.Distributions 
    (
    -- * Variates: non-uniformly distributed values
      normal
    , standard
    , exponential
    , gamma
    , chiSquare
    , geometric
    , geometric1

    -- * References
    -- $references
    ) where

import Control.Monad (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits ((.&.))
import Data.Word (Word32)
import System.Random.MWC (Gen, uniform)
import qualified Data.Vector.Unboxed as I

-- Unboxed 2-tuple
data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double


-- | Generate a normally distributed random variate with given mean
-- and standard deviation.
normal :: PrimMonad m
       => Double                -- ^ Mean
       -> Double                -- ^ Standard deviation
       -> Gen (PrimState m)
       -> m Double
{-# INLINE normal #-}
normal m s gen = do
  x <- standard gen
  return $! m + s * x

-- | Generate a normally distributed random variate with zero mean and
-- unit variance.
--
-- The implementation uses Doornik's modified ziggurat algorithm.
-- Compared to the ziggurat algorithm usually used, this is slower,
-- but generates more independent variates that pass stringent tests
-- of randomness.
standard :: PrimMonad m => Gen (PrimState m) -> m Double
{-# INLINE standard #-}
standard gen = loop
  where
    loop = do
      u  <- (subtract 1 . (*2)) `liftM` uniform gen
      ri <- uniform gen
      let i  = fromIntegral ((ri :: Word32) .&. 127)
          bi = I.unsafeIndex blocks i
          bj = I.unsafeIndex blocks (i+1)
      case () of
        _| abs u < I.unsafeIndex ratios i -> return $! u * bi
         | i == 0                         -> normalTail (u < 0)
         | otherwise                      -> do
             let x  = u * bi
                 xx = x * x
                 d  = exp (-0.5 * (bi * bi - xx))
                 e  = exp (-0.5 * (bj * bj - xx))
             c <- uniform gen
             if e + c * (d - e) < 1
               then return x
               else loop
    blocks = (`I.snoc` 0) . I.cons (v/f) . I.cons r . I.unfoldrN 126 go $! T r f
      where
        go (T b g)   = let !u = T h (exp (-0.5 * h * h))
                           h  = sqrt (-2 * log (v / b + g))
                       in Just (h, u)
        v            = 9.91256303526217e-3
        f            = exp (-0.5 * r * r)
    {-# NOINLINE blocks #-}
    r                = 3.442619855899
    ratios           = I.zipWith (/) (I.tail blocks) blocks
    {-# NOINLINE ratios #-}
    normalTail neg  = tailing
      where tailing  = do
              x <- ((/r) . log) `liftM` uniform gen
              y <- log          `liftM` uniform gen
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - r else r - x


-- | Generate an exponentially distributed random variate.
exponential :: PrimMonad m
            => Double            -- ^ Scale parameter
            -> Gen (PrimState m) -- ^ Generator
            -> m Double
{-# INLINE exponential #-}
exponential beta gen = do
  x <- uniform gen
  return $! - log x / beta


-- | Random variate generator for gamma distribution.
gamma :: PrimMonad m
      => Double                 -- ^ Shape parameter
      -> Double                 -- ^ Scale parameter
      -> Gen (PrimState m)      -- ^ Generator
      -> m Double
{-# INLINE gamma #-}
gamma a b gen
  | a <= 0    = pkgError "gamma" "negative alpha parameter"
  | otherwise = mainloop
    where
      mainloop = do
        T x v <- innerloop
        u     <- uniform gen
        let cont =  u > 1 - 0.331 * sqr (sqr x)
                 && log u > 0.5 * sqr x + a1 * (1 - v + log v) -- Rarely evaluated
        case () of
          _| cont      -> mainloop
           | a >= 1    -> return $! a1 * v * b
           | otherwise -> do y <- uniform gen
                             return $! y ** (1 / a) * a1 * v * b
      -- inner loop
      innerloop = do
        x <- standard gen
        case 1 + a2*x of
          v | v <= 0    -> innerloop
            | otherwise -> return $! T x (v*v*v)
      -- constants
      a' = if a < 1 then a + 1 else a
      a1 = a' - 1/3
      a2 = 1 / sqrt(9 * a1)


-- | Random variate generator for the chi square distribution.
chiSquare :: PrimMonad m
          => Int                -- ^ Number of degrees of freedom
          -> Gen (PrimState m)  -- ^ Generator
          -> m Double
{-# INLINE chiSquare #-}
chiSquare n gen
  | n <= 0    = pkgError "chiSquare" "number of degrees of freedom must be positive"
  | otherwise = do x <- gamma (0.5 * fromIntegral n) 1 gen
                   return $! 2 * x

-- | Random variate generator for geometrical distribution for number
--   of failures before success. Have support [0..]
geometric :: PrimMonad m
          => Double            -- ^ /p/ success probability lies in (0,1]
          -> Gen (PrimState m) -- ^ Generator
          -> m Int
{-# INLINE geometric #-}
geometric p gen
  | p == 1          = return 0
  | p >  0 && p < 1 = do q <- uniform gen
                         -- FIXME: We want to use log1p here but it will
                         --        introduce dependency on math-functions.
                         return $! floor $ log q / log (1 - p)
  | otherwise       = pkgError "geometrical" "probability out of [0,1] range"

-- | Random variate generator for geometrical distribution for number
--   of trial. Have support [1..]  it's just 'geometrical' shifted by 1.
geometric1 :: PrimMonad m
           => Double            -- ^ /p/ success probability lies in (0,1]
           -> Gen (PrimState m) -- ^ Generator
           -> m Int
{-# INLINE geometric1 #-}
geometric1 p gen = do n <- geometric p gen
                      return $! n + 1


sqr :: Double -> Double
sqr x = x * x
{-# INLINE sqr #-}

pkgError :: String -> String -> a
pkgError func msg = error $ "System.Random.MWC.Distributions." ++ func ++
                            ": " ++ msg

-- $references
--
-- * Doornik, J.A. (2005) An improved ziggurat method to generate
--   normal random samples. Mimeo, Nuffield College, University of
--   Oxford.  <http://www.doornik.com/research/ziggurat.pdf>
--
-- * Thomas, D.B.; Leong, P.G.W.; Luk, W.; Villasenor, J.D.
--   (2007). Gaussian random number generators.
--   /ACM Computing Surveys/ 39(4).
--   <http://www.cse.cuhk.edu.hk/~phwl/mt/public/archives/papers/grng_acmcs07.pdf>
