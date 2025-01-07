{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
-- |
-- Low level source of random values for seeds. It should work on both
-- unices and windows
module System.Random.MWC.SeedSource (
    acquireSeedSystem
  , acquireSeedTime
  , randomSourceName
  ) where

import Data.Word               (Word32,Word64)
import Data.Bits               (shiftR)
import Data.Ratio              ((%), numerator)
import Data.Time.Clock.POSIX   (getPOSIXTime)

import Foreign.Storable
import Foreign.Marshal.Alloc   (allocaBytes)
import Foreign.Marshal.Array   (peekArray)
#if defined(mingw32_HOST_OS)
import Foreign.Ptr
import Foreign.C.Types
#else
import System.IO        (IOMode(..), hGetBuf, withBinaryFile)
#endif
import System.CPUTime   (cpuTimePrecision, getCPUTime)

-- Acquire seed from current time. This is horrible fallback for
-- Windows system.
acquireSeedTime :: IO [Word32]
acquireSeedTime = do
  c <- numerator . (% cpuTimePrecision) <$> getCPUTime
  t <- toRational <$> getPOSIXTime
  let n    = fromIntegral (numerator t) :: Word64
  return [fromIntegral c, fromIntegral n, fromIntegral (n `shiftR` 32)]

-- | Acquire seed from the system entropy source. On Unix machines,
-- this will attempt to use @/dev/urandom@. On Windows, it will internally
-- use @RtlGenRandom@.
acquireSeedSystem :: forall a. Storable a => Int -> IO [a]
acquireSeedSystem nElts = do
  let eltSize = sizeOf (undefined :: a)
      nbytes  = nElts * eltSize
#if !defined(mingw32_HOST_OS)
  allocaBytes nbytes $ \buf -> do
    nread <- withBinaryFile "/dev/urandom" ReadMode $ \h -> hGetBuf h buf nbytes
    peekArray (nread `div` eltSize) buf
#else
  -- Generate 256 random Word32s from RtlGenRandom
  allocaBytes nbytes $ \buf -> do
    ok <- c_RtlGenRandom buf (fromIntegral nbytes)
    if ok then return () else fail "Couldn't use RtlGenRandom"
    peekArray nElts buf

-- Note: on 64-bit Windows, the 'stdcall' calling convention
-- isn't supported, so we use 'ccall' instead.
#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 architecture!
#endif

-- Note: On Windows, the typical convention would be to use
-- the CryptoGenRandom API in order to generate random data.
-- However, here we use 'SystemFunction036', AKA RtlGenRandom.
--
-- This is a commonly used API for this purpose; one bonus is
-- that it avoids having to bring in the CryptoAPI library,
-- and completely sidesteps the initialization cost of CryptoAPI.
--
-- While this function is technically "subject to change" that is
-- extremely unlikely in practice: rand_s in the Microsoft CRT uses
-- this, and they can't change it easily without also breaking
-- backwards compatibility with e.g. statically linked applications.
--
-- The name 'SystemFunction036' is the actual link-time name; the
-- display name is just for giggles, I guess.
--
-- See also:
--   - http://blogs.msdn.com/b/michael_howard/archive/2005/01/14/353379.aspx
--   - https://bugzilla.mozilla.org/show_bug.cgi?id=504270
--
foreign import WINDOWS_CCONV unsafe "SystemFunction036"
  c_RtlGenRandom :: Ptr a -> CULong -> IO Bool
#endif


-- | Name of source of randomness. It should be used in error messages
randomSourceName :: String
#if !defined(mingw32_HOST_OS)
randomSourceName = "/dev/urandom"
#else
randomSourceName = "RtlGenRandom"
#endif
