{-# LANGUAGE BangPatterns, GADTs, FlexibleContexts, ScopedTypeVariables #-}
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
    -- ** Continuous distributions
      normal
    , standard
    , exponential
    , truncatedExp
    , gamma
    , chiSquare
    , beta
      -- ** Discrete distribution
    , categorical
    , geometric0
    , geometric1
    , bernoulli
      -- ** Multivariate
    , dirichlet
      -- * Permutations
    , uniformPermutation
    , uniformShuffle

    -- * References
    -- $references
    ) where

import Prelude hiding (mapM)
import Control.Monad (liftM,when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits ((.&.))
import Data.Foldable (Foldable,foldl')
import Data.Traversable (Traversable,mapM)
import Data.Word (Word32)
import System.Random.MWC (Gen, uniform, uniformR)
import qualified Data.Vector.Unboxed         as I
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

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
-- We express standard in terms of normal and not other way round
-- because of bug in GHC. See bug #16 for more details.
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
    normalTail neg  = tailing
      where tailing  = do
              x <- ((/rNorm) . log) `liftM` uniform gen
              y <- log              `liftM` uniform gen
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - rNorm else rNorm - x

-- Constants used by standard/normal. They are floated to the top
-- level to avoid performance regression (Bug #16) when blocks/ratios
-- are recalculated on each call to standard/normal. It's also
-- somewhat difficult to trigger reliably.
blocks :: I.Vector Double
blocks = (`I.snoc` 0) . I.cons (v/f) . I.cons rNorm . I.unfoldrN 126 go $! T rNorm f
  where
    go (T b g) = let !u = T h (exp (-0.5 * h * h))
                     h  = sqrt (-2 * log (v / b + g))
                 in Just (h, u)
    v = 9.91256303526217e-3
    f = exp (-0.5 * rNorm * rNorm)
{-# NOINLINE blocks #-}

rNorm :: Double
rNorm = 3.442619855899

ratios :: I.Vector Double
ratios = I.zipWith (/) (I.tail blocks) blocks
{-# NOINLINE ratios #-}



-- | Generate an exponentially distributed random variate.
exponential :: PrimMonad m
            => Double            -- ^ Scale parameter
            -> Gen (PrimState m) -- ^ Generator
            -> m Double
{-# INLINE exponential #-}
exponential b gen = do
  x <- uniform gen
  return $! - log x / b


-- | Generate truncated exponentially distributed random variate.
truncatedExp :: PrimMonad m
             => Double            -- ^ Scale parameter
             -> (Double,Double)   -- ^ Range to which distribution is
                                  --   truncated. Values may be negative.
             -> Gen (PrimState m) -- ^ Generator.
             -> m Double
{-# INLINE truncatedExp #-}
truncatedExp scale (a,b) gen = do
  -- We shift a to 0 and then generate distribution truncated to [0,b-a]
  -- It's easier
  let delta = b - a
  p <- uniform gen
  return $! a - log ( (1 - p) + p*exp(-scale*delta)) / scale

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

-- | Random variate generator for the geometric distribution,
-- computing the number of failures before success. Distribution's
-- support is [0..].
geometric0 :: PrimMonad m
           => Double            -- ^ /p/ success probability lies in (0,1]
           -> Gen (PrimState m) -- ^ Generator
           -> m Int
{-# INLINE geometric0 #-}
geometric0 p gen
  | p == 1          = return 0
  | p >  0 && p < 1 = do q <- uniform gen
                         -- FIXME: We want to use log1p here but it will
                         --        introduce dependency on math-functions.
                         return $! floor $ log q / log (1 - p)
  | otherwise       = pkgError "geometric0" "probability out of [0,1] range"

-- | Random variate generator for geometric distribution for number of
-- trials. Distribution's support is [1..] (i.e. just 'geometric0'
-- shifted by 1).
geometric1 :: PrimMonad m
           => Double            -- ^ /p/ success probability lies in (0,1]
           -> Gen (PrimState m) -- ^ Generator
           -> m Int
{-# INLINE geometric1 #-}
geometric1 p gen = do n <- geometric0 p gen
                      return $! n + 1

-- | Random variate generator for Beta distribution
beta :: PrimMonad m
     => Double            -- ^ alpha (>0)
     -> Double            -- ^ beta  (>0)
     -> Gen (PrimState m) -- ^ Generator
     -> m Double
{-# INLINE beta #-}
beta a b gen = do
  x <- gamma a 1 gen
  y <- gamma b 1 gen
  return $! x / (x+y)

-- | Random variate generator for Dirichlet distribution
dirichlet :: (PrimMonad m,Foldable t,Traversable t)
          => t Double          -- ^ container of parameters
          -> Gen (PrimState m) -- ^ Generator
          -> m (t Double)
{-# INLINE dirichlet #-}
dirichlet t gen = do
  t' <- mapM (\x -> gamma x 1 gen) t
  let total = foldl' (+) 0 t'
  return $ fmap (/total) t'

-- | Random variate generator for Bernoulli distribution
bernoulli :: PrimMonad m
          => Double            -- ^ Probability of success (returning True)
          -> Gen (PrimState m) -- ^ Generator
          -> m Bool
{-# INLINE bernoulli #-}
bernoulli p gen = (<p) `liftM` uniform gen

-- | Random variate generator for categorical distribution.
--
--   Note that if you need to generate a lot of variates functions
--   "System.Random.MWC.CondensedTable" will offer better
--   performance.  If only few is needed this function will faster
--   since it avoids costs of setting up table.
categorical :: (PrimMonad m, G.Vector v Double)
            => v Double          -- ^ List of weights [>0]
            -> Gen (PrimState m) -- ^ Generator
            -> m Int
categorical v gen
    | G.null v = pkgError "categorical" "empty weights!"
    | otherwise = do
        let cv  = G.scanl1' (+) v
        p <- (G.last cv *) `liftM` uniform gen
        return $! case G.findIndex (>=p) cv of
                    Just i  -> i
                    Nothing -> pkgError "categorical" "bad weights!"

-- | Random variate generator for uniformly distributed permutations.
--   It returns random permutation of vector /[0 .. n-1]/.
--
--   This is the Fisher-Yates shuffle
uniformPermutation :: forall m v. (PrimMonad m, G.Vector v Int)
                   => Int
                   -> Gen (PrimState m)
                   -> m (v Int)
{-# INLINE uniformPermutation #-}
uniformPermutation n gen = do
  when (n<=0) (pkgError "uniformPermutation" "size must be >0")
  v <- G.unsafeThaw (G.generate n id :: v Int)
  let lst = n-1
      loop i | i == lst  = G.unsafeFreeze v
             | otherwise = do
                 j <- uniformR (i,lst) gen
                 M.unsafeSwap v i j
                 loop (i+1)
  loop 0


-- | Random variate generator for a uniformly distributed shuffle of a
--   vector.
uniformShuffle :: (PrimMonad m, G.Vector v a, G.Vector v Int)
               => v a
               -> Gen (PrimState m)
               -> m (v a)
{-# INLINE uniformShuffle #-}
uniformShuffle xs gen
    | G.length xs <= 1 = return xs
    | otherwise        = do
        idx <- uniformPermutation (G.length xs) gen
        return $! G.backpermute xs idx


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
