import Control.Exception
import Control.Monad.ST
import Criterion.Main
import Data.Int
import Data.Word
import qualified System.Random as R
import System.Random.MWC
import System.Random.MWC.Distributions
import qualified System.Random.Mersenne as M

main = do
  mwc <- create
  mtg <- M.newMTGen . Just =<< uniform mwc
  defaultMain 
    [ bgroup "mwc"
      -- One letter group names are used so they will fit on the plot.
      --
      --  U - uniform
      --  R - uniformR
      --  D - distribution
      [ bgroup "U"
        [ bench "Double"  (uniform mwc :: IO Double)
        , bench "Int"     (uniform mwc :: IO Int)
        , bench "Int8"    (uniform mwc :: IO Int8)
        , bench "Int16"   (uniform mwc :: IO Int16)
        , bench "Int32"   (uniform mwc :: IO Int32)
        , bench "Int64"   (uniform mwc :: IO Int64)
        , bench "Word"    (uniform mwc :: IO Word)
        , bench "Word8"   (uniform mwc :: IO Word8)
        , bench "Word16"  (uniform mwc :: IO Word16)
        , bench "Word32"  (uniform mwc :: IO Word32)
        , bench "Word64"  (uniform mwc :: IO Word64)
        ]
      , bgroup "R"
        -- I'm not entirely convinced that this is right way to test
        -- uniformR. /A.Khudyakov/
        [ bench "Double"  (uniformR (-3.21,26) mwc :: IO Double)
        , bench "Int"     (uniformR (-12,679)  mwc :: IO Int)
        , bench "Int8"    (uniformR (-12,4)    mwc :: IO Int8)
        , bench "Int16"   (uniformR (-12,679)  mwc :: IO Int16)
        , bench "Int32"   (uniformR (-12,679)  mwc :: IO Int32)
        , bench "Int64"   (uniformR (-12,679)  mwc :: IO Int64)
        , bench "Word"    (uniformR (34,633)   mwc :: IO Word)
        , bench "Word8"   (uniformR (34,63)    mwc :: IO Word8)
        , bench "Word16"  (uniformR (34,633)   mwc :: IO Word16)
        , bench "Word32"  (uniformR (34,633)   mwc :: IO Word32)
        , bench "Word64"  (uniformR (34,633)   mwc :: IO Word64)
        ]
      , bgroup "D"
        [ bench "standard"    (standard      mwc :: IO Double)
        , bench "normal"      (normal 1 3    mwc :: IO Double)
        , bench "exponential" (exponential 3 mwc :: IO Double)
        , bench "gamma,a<1"   (gamma 0.5 1   mwc :: IO Double)
        , bench "gamma,a>1"   (gamma 2   1   mwc :: IO Double)
        , bench "chiSquare"   (chiSquare 4   mwc :: IO Double)
        ]
      ]
    , bgroup "random"
      [
        bench "Double" (R.randomIO >>= evaluate :: IO Double)
      , bench "Int" (R.randomIO >>= evaluate :: IO Int)
      ]
    , bgroup "mersenne"
      [
        bench "Double" (M.random mtg :: IO Double)
      , bench "Int" (M.random mtg :: IO Int)
      ]
    ]
