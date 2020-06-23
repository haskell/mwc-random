{-# LANGUAGE BangPatterns #-}
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Gauge.Main
import Data.Int
import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified System.Random as R
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.MWC.CondensedTable
import qualified System.Random.Mersenne as M

makeTableUniform :: Int -> CondensedTable U.Vector Int
makeTableUniform n =
  tableFromProbabilities $ U.zip (U.enumFromN 0 n) (U.replicate n (1 / fromIntegral n))
{-# INLINE makeTableUniform #-}


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
        [ bench "Double"  $ nfIO (uniform mwc :: IO Double)
        , bench "Int"     $ nfIO (uniform mwc :: IO Int)
        , bench "Int8"    $ nfIO (uniform mwc :: IO Int8)
        , bench "Int16"   $ nfIO (uniform mwc :: IO Int16)
        , bench "Int32"   $ nfIO (uniform mwc :: IO Int32)
        , bench "Int64"   $ nfIO (uniform mwc :: IO Int64)
        , bench "Word"    $ nfIO (uniform mwc :: IO Word)
        , bench "Word8"   $ nfIO (uniform mwc :: IO Word8)
        , bench "Word16"  $ nfIO (uniform mwc :: IO Word16)
        , bench "Word32"  $ nfIO (uniform mwc :: IO Word32)
        , bench "Word64"  $ nfIO (uniform mwc :: IO Word64)
        ]
      , bgroup "R"
        -- I'm not entirely convinced that this is right way to test
        -- uniformR. /A.Khudyakov/
        [ bench "Double"  $ nfIO (uniformR (-3.21,26) mwc :: IO Double)
        , bench "Int"     $ nfIO (uniformR (-12,679)  mwc :: IO Int)
        , bench "Int8"    $ nfIO (uniformR (-12,4)    mwc :: IO Int8)
        , bench "Int16"   $ nfIO (uniformR (-12,679)  mwc :: IO Int16)
        , bench "Int32"   $ nfIO (uniformR (-12,679)  mwc :: IO Int32)
        , bench "Int64"   $ nfIO (uniformR (-12,679)  mwc :: IO Int64)
        , bench "Word"    $ nfIO (uniformR (34,633)   mwc :: IO Word)
        , bench "Word8"   $ nfIO (uniformR (34,63)    mwc :: IO Word8)
        , bench "Word16"  $ nfIO (uniformR (34,633)   mwc :: IO Word16)
        , bench "Word32"  $ nfIO (uniformR (34,633)   mwc :: IO Word32)
        , bench "Word64"  $ nfIO (uniformR (34,633)   mwc :: IO Word64)
        ]
      , bgroup "D"
        [ bench "standard"    $ nfIO (standard      mwc :: IO Double)
        , bench "normal"      $ nfIO (normal 1 3    mwc :: IO Double)
          -- Regression tests for #16. These functions should take 10x
          -- longer to execute.
          --
          -- N.B. Bang patterns are necessary to trigger the bug with
          --      GHC 7.6
        , bench "standard/N"  $ nfIO $ replicateM_ 10 $ do
                                 !_ <- standard mwc :: IO Double
                                 return ()
        , bench "normal/N"    $ nfIO $ replicateM_ 10 $ do
                                 !_ <- normal 1 3 mwc :: IO Double
                                 return ()
        , bench "exponential" $ nfIO (exponential 3 mwc :: IO Double)
        , bench "gamma,a<1"   $ nfIO (gamma 0.5 1   mwc :: IO Double)
        , bench "gamma,a>1"   $ nfIO (gamma 2   1   mwc :: IO Double)
        , bench "chiSquare"   $ nfIO (chiSquare 4   mwc :: IO Double)
        ]
      , bgroup "CT/gen" $ concat
        [ [ bench ("uniform "++show i) $ nfIO (genFromTable (makeTableUniform i) mwc :: IO Int)
          | i <- [2..10]
          ]
        , [ bench ("poisson " ++ show l) $ nfIO (genFromTable (tablePoisson l) mwc :: IO Int)
          | l <- [0.01, 0.2, 0.8, 1.3, 2.4, 8, 12, 100, 1000]
          ]
        , [ bench ("binomial " ++ show p ++ " " ++ show n) $ nfIO (genFromTable (tableBinomial n p) mwc :: IO Int)
          | (n,p) <- [ (4, 0.5), (10,0.1), (10,0.6), (10, 0.8), (100,0.4)]
          ]
        ]
      , bgroup "CT/table" $ concat
        [ [ bench ("uniform " ++ show i) $ whnf makeTableUniform i
          | i <- [2..30]
          ]
        , [ bench ("poisson " ++ show l) $ whnf tablePoisson l
          | l <- [0.01, 0.2, 0.8, 1.3, 2.4, 8, 12, 100, 1000]
          ]
        , [ bench ("binomial " ++ show p ++ " " ++ show n) $ whnf (tableBinomial n) p
          | (n,p) <- [ (4, 0.5), (10,0.1), (10,0.6), (10, 0.8), (100,0.4)]
          ]
        ]
      ]
    , bgroup "random"
      [
        bench "Double" $ nfIO (R.randomIO >>= evaluate :: IO Double)
      , bench "Int"    $ nfIO (R.randomIO >>= evaluate :: IO Int)
      ]
    , bgroup "mersenne"
      [
        bench "Double" $ nfIO (M.random mtg :: IO Double)
      , bench "Int"    $ nfIO (M.random mtg :: IO Int)
      ]
    ]
