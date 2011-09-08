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
      , bench "normal"      (normal        mwc :: IO Double)
      , bench "exponential" (exponential 3 mwc :: IO Double)
      , bench "gamma,a<1"   (gamma 0.5 1   mwc :: IO Double)
      , bench "gamma,a>1"   (gamma 2   1   mwc :: IO Double)
      , bench "chiSquare"   (chiSquare 4   mwc :: IO Double)
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
