{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, MagicHash, Rank2Types,
    ScopedTypeVariables #-}
-- |
-- Module    : System.Random.MWC
-- Copyright : (c) 2009, 2010 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation.  This module contains code for
-- generating high quality random numbers that follow either a uniform
-- or normal distribution.
--
-- The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
-- multiply-with-carry generator, which has a period of 2^8222 and
-- fares well in tests of randomness.  It is also extremely fast,
-- between 2 and 3 times faster than the Mersenne Twister.
module System.Random.MWC
    (
    -- * Types
      Gen
    , GenIO
    , GenST
    , Seed
    , Variate(..)
    -- * Other distributions
    , normal
    -- * Creation
    , create
    , initialize
    , withSystemRandom
    -- * State management
    , save
    , restore
    -- * Helper functions
    , uniformVector
    -- * References
    -- $references
    ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import Control.Exception (IOException, catch)
import Control.Monad (ap, liftM, unless)
import Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToIO)
import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.), xor)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IORef (atomicModifyIORef, newIORef)
import Data.Ratio ((%), numerator)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (Typeable)
import Data.Vector.Generic (Vector, unsafeFreeze)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import GHC.Base (Int(I#))
import GHC.Word (Word64(W64#), uncheckedShiftL64#, uncheckedShiftRL64#)
import Prelude hiding (catch)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as I
import qualified Data.Vector.Unboxed.Mutable as M
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.IO (IOMode(..), hGetBuf, hPutStrLn, stderr, withBinaryFile)
import System.IO.Unsafe (unsafePerformIO)

-- | The class of types for which we can generate uniformly
-- distributed random variates.
--
-- The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
-- multiply-with-carry generator, which has a period of 2^8222 and
-- fares well in tests of randomness.  It is also extremely fast,
-- between 2 and 3 times faster than the Mersenne Twister.
--
-- /Note/: Marsaglia's PRNG is not known to be cryptographically
-- secure, so you should not use it for cryptographic operations.
class M.Unbox a => Variate a where
    -- | Generate a single uniformly distributed random variate.  The
    -- range of values produced varies by type:
    --
    -- * For fixed-width integral types, the type's entire range is
    --   used.
    --
    -- * For floating point numbers, the range (0,1] is used. Zero is
    --   explicitly excluded, to allow variates to be used in
    --   statistical calculations that require non-zero values
    --   (e.g. uses of the 'log' function).
    --
    -- * The range of random 'Integer' variates is the same as for
    --   'Int'.
    --
    -- To generate a 'Float' variate with a range of [0,1), subtract
    -- 2**(-33).  To do the same with 'Double' variates, subtract
    -- 2**(-53).
    uniform :: (PrimMonad m) => Gen (PrimState m) -> m a

instance Variate Int8 where
    uniform = uniform1 fromIntegral
    {-# INLINE uniform #-}

instance Variate Int16 where
    uniform = uniform1 fromIntegral
    {-# INLINE uniform #-}

instance Variate Int32 where
    uniform = uniform1 fromIntegral
    {-# INLINE uniform #-}

instance Variate Int64 where
    uniform = uniform2 wordsTo64Bit
    {-# INLINE uniform #-}

instance Variate Word8 where
    uniform = uniform1 fromIntegral
    {-# INLINE uniform #-}

instance Variate Word16 where
    uniform = uniform1 fromIntegral
    {-# INLINE uniform #-}

instance Variate Word32 where
    uniform = uniformWord32
    {-# INLINE uniform #-}

instance Variate Word64 where
    uniform = uniform2 wordsTo64Bit
    {-# INLINE uniform #-}

instance Variate Bool where
    uniform = uniform1 wordToBool
    {-# INLINE uniform #-}

instance Variate Float where
    uniform = uniform1 wordToFloat
    {-# INLINE uniform #-}

instance Variate Double where
    uniform = uniform2 wordsToDouble
    {-# INLINE uniform #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    {-# INLINE uniform #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    {-# INLINE uniform #-}

{-
instance Variate Integer where
    uniform g = do
      u <- uniform g
      return $! fromIntegral (u :: Int)
    {-# INLINE uniform #-}
-}

instance (Variate a, Variate b) => Variate (a,b) where
    uniform g = (,) `liftM` uniform g `ap` uniform g
    {-# INLINE uniform #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
    uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
    {-# INLINE uniform #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
    uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g
                `ap` uniform g
    {-# INLINE uniform #-}

wordsTo64Bit :: Integral a => Word32 -> Word32 -> a
wordsTo64Bit a b =
    fromIntegral ((fromIntegral a `shiftL` 32) .|. fromIntegral b)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral a * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (b .&. 0xFFFFF) * m_inv_52) 
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          a        = fromIntegral x :: Int32
          b        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

-- | State of the pseudo-random number generator.
newtype Gen s = Gen (M.MVector s Word32)

-- | A shorter name for PRNG state in the IO monad.
type GenIO = Gen (PrimState IO)

-- | A shorter name for PRNG state in the ST monad.
type GenST s = Gen (PrimState (ST s))

ioff, coff :: Int
ioff = 256
coff = 257

-- | Create a generator for variates using a fixed seed.
create :: PrimMonad m => m (Gen (PrimState m))
create = initialize defaultSeed
{-# INLINE create #-}

-- | Create a generator for variates using the given seed, of which up
-- to 256 elements will be used.  For arrays of less than 256
-- elements, part of the default seed will be used to finish
-- initializing the generator's state.
--
-- Examples:
--
-- > initialize (singletonU 42)
--
-- > initialize (toU [4, 8, 15, 16, 23, 42])
--
-- If a seed contains fewer than 256 elements, it is first used
-- verbatim, then its elements are 'xor'ed against elements of the
-- default seed until 256 elements are reached.
initialize :: PrimMonad m => I.Vector Word32 -> m (Gen (PrimState m))
initialize seed = do
    q <- M.unsafeNew 258
    fill q
    M.unsafeWrite q ioff 255
    M.unsafeWrite q coff 362436
    return (Gen q)
  where fill q = go 0 where
          go i | i == 256  = return ()
               | otherwise = M.unsafeWrite q i s >> go (i+1)
            where s | i >= fini = if fini == 0
                                  then I.unsafeIndex defaultSeed i
                                  else I.unsafeIndex defaultSeed i `xor`
                                       I.unsafeIndex seed (i `mod` fini)
                    | otherwise = I.unsafeIndex seed i
        fini = I.length seed
{-# INLINE initialize #-}
                               
-- | An immutable snapshot of the state of a 'Gen'.
newtype Seed = Seed (I.Vector Word32)
    deriving (Eq, Show, Typeable)

-- Safe version of unsafeFreeze.
-- NOTE: vector-0.7 will provide function `freeze' with same
--       functionality. This function shall be removed when support for
--       vector<=0.6 is droppedA
safeFreeze :: (PrimMonad m, Vector v a) => G.Mutable v (PrimState m) a -> m (v a)
safeFreeze v = do
  v' <- GM.unsafeNew (GM.length v)
  GM.unsafeCopy v' v
  unsafeFreeze v'

-- | Save the state of a 'Gen', for later use by 'restore'.
save :: PrimMonad m => Gen (PrimState m) -> m Seed
save (Gen q) = Seed `liftM` safeFreeze q
{-# INLINE save #-}

-- | Create a new 'Gen' that mirrors the state of a saved 'Seed'.
restore :: PrimMonad m => Seed -> m (Gen (PrimState m))
restore (Seed s) = M.unsafeNew n >>= fill
  where fill q = go 0 where
          go !i | i >= n    = return $! Gen q
                | otherwise = M.unsafeWrite q i (I.unsafeIndex s i) >> go (i+1)
        n = I.length s
{-# INLINE restore #-}
  
-- | Using the current time as a seed, perform an action that uses a
-- random variate generator.  This is a horrible fallback for Windows
-- systems.
withTime :: (PrimMonad m) => (Gen (PrimState m) -> m a) -> IO a
withTime act = do
  c <- (numerator . (%cpuTimePrecision)) `liftM` getCPUTime
  t <- toRational `liftM` getPOSIXTime
  let n    = fromIntegral (numerator t) :: Word64
      seed = [fromIntegral c, fromIntegral n, fromIntegral (n `shiftR` 32)]
  unsafePrimToIO $ initialize (I.fromList seed) >>= act

-- | Seed a PRNG with data from the system's fast source of
-- pseudo-random numbers (\"\/dev\/urandom\" on Unix-like systems),
-- then run the given action.
--
-- /Note/: on Windows, this code does not yet use the native
-- Cryptographic API as a source of random numbers (it uses the system
-- clock instead). As a result, the sequences it generates may not be
-- highly independent.
withSystemRandom :: PrimMonad m => (Gen (PrimState m) -> m a) -> IO a
withSystemRandom act = tryRandom `catch` \(_::IOException) -> do
    seen <- atomicModifyIORef warned ((,) True)
    unless seen $ do
      hPutStrLn stderr ("Warning: Couldn't open " ++ show random)
      hPutStrLn stderr ("Warning: using system clock for seed instead " ++
                        "(quality will be lower)")
    withTime act
  where tryRandom = do
          let nbytes = 1024
          ws <- allocaBytes nbytes $ \buf -> do
                  nread <- withBinaryFile random ReadMode $
                           \h -> hGetBuf h buf nbytes
                  peekArray (nread `div` 4) buf
          unsafePrimToIO $ initialize (I.fromList ws) >>= act
        random = "/dev/urandom"
        warned = unsafePerformIO $ newIORef False
        {-# NOINLINE warned #-}

-- | Unchecked 64-bit left shift.
shiftL :: Word64 -> Int -> Word64
shiftL (W64# x#) (I# i#) = W64# (x# `uncheckedShiftL64#` i#)

-- | Unchecked 64-bit right shift.
shiftR :: Word64 -> Int -> Word64
shiftR (W64# x#) (I# i#) = W64# (x# `uncheckedShiftRL64#` i#)

-- | Compute the next index into the state pool.  This is simply
-- addition modulo 256.
nextIndex :: Integral a => a -> Int
nextIndex i = fromIntegral j
    where j = fromIntegral (i+1) :: Word8

uniformWord32 :: PrimMonad m => Gen (PrimState m) -> m Word32
uniformWord32 (Gen q) = do
  let a = 809430660 :: Word64
  i <- nextIndex `liftM` M.unsafeRead q ioff
  c <- fromIntegral `liftM` M.unsafeRead q coff
  qi <- fromIntegral `liftM` M.unsafeRead q i
  let t   = a * qi + c
      t32 = fromIntegral t
  M.unsafeWrite q i t32
  M.unsafeWrite q ioff (fromIntegral i)
  M.unsafeWrite q coff (fromIntegral (t `shiftR` 32))
  return t32
{-# INLINE uniformWord32 #-}

uniform1 :: PrimMonad m => (Word32 -> a) -> Gen (PrimState m) -> m a
uniform1 f gen = do
  i <- uniformWord32 gen
  return $! f i
{-# INLINE uniform1 #-}

uniform2 :: PrimMonad m => (Word32 -> Word32 -> a) -> Gen (PrimState m) -> m a
uniform2 f (Gen q) = do
  let a = 809430660 :: Word64
  i <- nextIndex `liftM` M.unsafeRead q ioff
  let j = nextIndex i
  c <- fromIntegral `liftM` M.unsafeRead q coff
  qi <- fromIntegral `liftM` M.unsafeRead q i
  qj <- fromIntegral `liftM` M.unsafeRead q j
  let t   = a * qi + c
      t32 = fromIntegral t
      c'  = t `shiftR` 32
      u   = a * qj + c'
      u32 = fromIntegral u
  M.unsafeWrite q i t32
  M.unsafeWrite q j u32
  M.unsafeWrite q ioff (fromIntegral j)
  M.unsafeWrite q coff (fromIntegral (u `shiftR` 32))
  return $! f t32 u32
{-# INLINE uniform2 #-}

-- | Generate a vector of pseudo-random variates.  This is not
-- necessarily faster than invoking 'uniform' repeatedly in a loop,
-- but it may be more convenient to use in some situations.
uniformVector :: (PrimMonad m, Variate a)
             => Gen (PrimState m) -> Int -> m (I.Vector a)
uniformVector gen n = do
  mu <- M.unsafeNew n
  let go !i | i < n     = uniform gen >>= M.unsafeWrite mu i >> go (i+1)
            | otherwise = unsafeFreeze mu
  go 0
{-# INLINE uniformVector #-}

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
    r                = 3.442619855899
    ratios           = I.zipWith (/) (I.tail blocks) blocks
    normalTail neg   = tailing
      where tailing  = do
              x <- ((/r) . log) `liftM` uniform gen
              y <- log          `liftM` uniform gen
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - r else r - x
{-# INLINE normal #-}

defaultSeed :: I.Vector Word32
defaultSeed = I.fromList [
  0x7042e8b3, 0x06f7f4c5, 0x789ea382, 0x6fb15ad8, 0x54f7a879, 0x0474b184,
  0xb3f8f692, 0x4114ea35, 0xb6af0230, 0xebb457d2, 0x47693630, 0x15bc0433,
  0x2e1e5b18, 0xbe91129c, 0xcc0815a0, 0xb1260436, 0xd6f605b1, 0xeaadd777,
  0x8f59f791, 0xe7149ed9, 0x72d49dd5, 0xd68d9ded, 0xe2a13153, 0x67648eab,
  0x48d6a1a1, 0xa69ab6d7, 0x236f34ec, 0x4e717a21, 0x9d07553d, 0x6683a701,
  0x19004315, 0x7b6429c5, 0x84964f99, 0x982eb292, 0x3a8be83e, 0xc1df1845,
  0x3cf7b527, 0xb66a7d3f, 0xf93f6838, 0x736b1c85, 0x5f0825c1, 0x37e9904b,
  0x724cd7b3, 0xfdcb7a46, 0xfdd39f52, 0x715506d5, 0xbd1b6637, 0xadabc0c0,
  0x219037fc, 0x9d71b317, 0x3bec717b, 0xd4501d20, 0xd95ea1c9, 0xbe717202,
  0xa254bd61, 0xd78a6c5b, 0x043a5b16, 0x0f447a25, 0xf4862a00, 0x48a48b75,
  0x1e580143, 0xd5b6a11b, 0x6fb5b0a4, 0x5aaf27f9, 0x668bcd0e, 0x3fdf18fd,
  0x8fdcec4a, 0x5255ce87, 0xa1b24dbf, 0x3ee4c2e1, 0x9087eea2, 0xa4131b26,
  0x694531a5, 0xa143d867, 0xd9f77c03, 0xf0085918, 0x1e85071c, 0x164d1aba,
  0xe61abab5, 0xb8b0c124, 0x84899697, 0xea022359, 0x0cc7fa0c, 0xd6499adf,
  0x746da638, 0xd9e5d200, 0xefb3360b, 0x9426716a, 0xabddf8c2, 0xdd1ed9e4,
  0x17e1d567, 0xa9a65000, 0x2f37dbc5, 0x9a4b8fd5, 0xaeb22492, 0x0ebe8845,
  0xd89dd090, 0xcfbb88c6, 0xb1325561, 0x6d811d90, 0x03aa86f4, 0xbddba397,
  0x0986b9ed, 0x6f4cfc69, 0xc02b43bc, 0xee916274, 0xde7d9659, 0x7d3afd93,
  0xf52a7095, 0xf21a009c, 0xfd3f795e, 0x98cef25b, 0x6cb3af61, 0x6fa0e310,
  0x0196d036, 0xbc198bca, 0x15b0412d, 0xde454349, 0x5719472b, 0x8244ebce,
  0xee61afc6, 0xa60c9cb5, 0x1f4d1fd0, 0xe4fb3059, 0xab9ec0f9, 0x8d8b0255,
  0x4e7430bf, 0x3a22aa6b, 0x27de22d3, 0x60c4b6e6, 0x0cf61eb3, 0x469a87df,
  0xa4da1388, 0xf650f6aa, 0x3db87d68, 0xcdb6964c, 0xb2649b6c, 0x6a880fa9,
  0x1b0c845b, 0xe0af2f28, 0xfc1d5da9, 0xf64878a6, 0x667ca525, 0x2114b1ce,
  0x2d119ae3, 0x8d29d3bf, 0x1a1b4922, 0x3132980e, 0xd59e4385, 0x4dbd49b8,
  0x2de0bb05, 0xd6c96598, 0xb4c527c3, 0xb5562afc, 0x61eeb602, 0x05aa192a,
  0x7d127e77, 0xc719222d, 0xde7cf8db, 0x2de439b8, 0x250b5f1a, 0xd7b21053,
  0xef6c14a1, 0x2041f80f, 0xc287332e, 0xbb1dbfd3, 0x783bb979, 0x9a2e6327,
  0x6eb03027, 0x0225fa2f, 0xa319bc89, 0x864112d4, 0xfe990445, 0xe5e2e07c,
  0xf7c6acb8, 0x1bc92142, 0x12e9b40e, 0x2979282d, 0x05278e70, 0xe160ba4c,
  0xc1de0909, 0x458b9bf4, 0xbfce9c94, 0xa276f72a, 0x8441597d, 0x67adc2da,
  0x6162b854, 0x7f9b2f4a, 0x0d995b6b, 0x193b643d, 0x399362b3, 0x8b653a4b,
  0x1028d2db, 0x2b3df842, 0x6eecafaf, 0x261667e9, 0x9c7e8cda, 0x46063eab,
  0x7ce7a3a1, 0xadc899c9, 0x017291c4, 0x528d1a93, 0x9a1ee498, 0xbb7d4d43,
  0x7837f0ed, 0x34a230cc, 0x614a628d, 0xb03f93b8, 0xd72e3b08, 0x604c98db,
  0x3cfacb79, 0x8b81646a, 0xc0f082fa, 0xd1f92388, 0xe5a91e39, 0xf95c756d,
  0x1177742f, 0xf8819323, 0x5c060b80, 0x96c1cd8f, 0x47d7b440, 0xbbb84197,
  0x35f749cc, 0x95b0e132, 0x8d90ad54, 0x5c3f9423, 0x4994005b, 0xb58f53b9,
  0x32df7348, 0x60f61c29, 0x9eae2f32, 0x85a3d398, 0x3b995dd4, 0x94c5e460,
  0x8e54b9f3, 0x87bc6e2a, 0x90bbf1ea, 0x55d44719, 0x2cbbfe6e, 0x439d82f0,
  0x4eb3782d, 0xc3f1e669, 0x61ff8d9e, 0x0909238d, 0xef406165, 0x09c1d762,
  0x705d184f, 0x188f2cc4, 0x9c5aa12a, 0xc7a5d70e, 0xbc78cb1b, 0x1d26ae62,
  0x23f96ae3, 0xd456bf32, 0xe4654f55, 0x31462bd8 ]

-- $references
--
-- * Doornik, J.A. (2005) An improved ziggurat method to generate
--   normal random samples. Mimeo, Nuffield College, University of
--   Oxford.  <http://www.doornik.com/research/ziggurat.pdf>
--
-- * Doornik, J.A. (2007) Conversion of high-period random numbers to
--   floating point.
--   /ACM Transactions on Modeling and Computer Simulation/ 17(1).
--   <http://www.doornik.com/research/randomdouble.pdf>
--
-- * Marsaglia, G. (2003) Seeds for random number generators.
--   /Communications of the ACM/ 46(5):90&#8211;93.
--   <http://doi.acm.org/10.1145/769800.769827>
--
-- * Thomas, D.B.; Leong, P.G.W.; Luk, W.; Villasenor, J.D.
--   (2007). Gaussian random number generators.
--   /ACM Computing Surveys/ 39(4).
--   <http://www.cse.cuhk.edu.hk/~phwl/mt/public/archives/papers/grng_acmcs07.pdf>
