
module System.Random.MWC.Distributions 
    (
      normal
    , exponential
    , gamma
    ) where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToIO)

import Data.Bits               ((.&.))
import Data.Word               (Word32)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as I
import qualified Data.Vector.Unboxed.Mutable as M

import System.Random.MWC


data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Generate a normally distributed random variate.
--
-- The implementation uses Doornik's modified ziggurat algorithm.
-- Compared to the ziggurat algorithm usually used, this is slower,
-- but generates more independent variates that pass stringent tests
-- of randomness.
normal :: PrimMonad m => Gen (PrimState m) -> m Double
normal gen = loop
  where
    loop = do
      u  <- (subtract 1 . (*2)) `liftM` uniform gen
      ri <- uniform gen
      let i  = fromIntegral ((ri :: Word32) .&. 127)
          bi = I.unsafeIndex blocks i
          bj = I.unsafeIndex blocks (i+1)
      if abs u < I.unsafeIndex ratios i
        then return $! u * bi
        else if i == 0
        then normalTail (u < 0)
        else do
          let x  = u * bi
              xx = x * x
              d  = exp (-0.5 * (bi * bi - xx))
              e  = exp (-0.5 * (bj * bj - xx))
          c <- uniform gen
          if e + c * (d - e) < 1
            then return x
            else loop
    blocks = let f = exp (-0.5 * r * r)
             in (`I.snoc` 0) . I.cons (v/f) . I.cons r .
                I.unfoldrN 126 go $! T r f
      where
        go (T b g)   = let !u = T h (exp (-0.5 * h * h))
                           h  = sqrt (-2 * log (v / b + g))
                       in Just (h, u)
        v            = 9.91256303526217e-3
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
{-# INLINE normal #-}

-- | Generate exponentially distributed random variate
exponential :: PrimMonad m
            => Double            -- ^ Scale parameter
            -> Gen (PrimState m) -- ^ Generator
            -> m Double
exponential beta gen = do
  x <- uniform gen
  return $! - log x / beta

-- | Random variate generator for gamma distribution
gamma :: PrimMonad m
      => Double                 -- ^ Shape parameter
      -> Double                 -- ^ Scale parameter
      -> Gen (PrimState m)      -- ^ Generator
      -> m Double
gamma a b gen
  | a <= 0    = error "System.Random.MWC.gamma: negative alpha parameter"
  | otherwise = mainloop
    where
      mainloop = do
        T x v <- innerloop
        u     <- uniform gen
        let cont =  u > 1 - 0.331 * sqr (sqr x)
                 && log u > 0.5 * sqr x + a1 * (1 - v + log v) -- Rarely evaluated
        case () of
          _| cont      -> mainloop
           | a >= 1    -> return $! a1 * v / b
           | otherwise -> do u <- uniform gen
                             return $! u ** (1 / a) * a1 * v / b
      -- inner loop
      innerloop = do
        x <- normal gen
        case 1 + a2*x of
          v | v <= 0    -> innerloop
            | otherwise -> return $! T x (v*v*v)
      -- constants
      a' = if a < 1 then a + 1 else a
      a1 = a' - 1/3
      a2 = 1 / sqrt(9 * a1)



sqr :: Double -> Double
sqr x = x * x
