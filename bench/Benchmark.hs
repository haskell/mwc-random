{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Main(main) where

import Control.Exception
import Data.Int
import Data.Word
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import qualified System.Random as R
import System.Random.Stateful (StatefulGen)
import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.MWC.CondensedTable
import qualified System.Random.Mersenne as M

import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty         (includingOptions)
import Bench

-- | Size of vector used in benchmarks
newtype Iterations = Iterations Int

instance IsOption Iterations where
  defaultValue = Iterations 10000
  parseValue = fmap Iterations . safeRead
  optionName = pure "iter"
  optionHelp = pure "Number of iteration in sampling benchmarks"


loop :: Iterations -> IO a -> IO ()
{-# INLINE loop #-}
loop (Iterations n) act = go n where
  go i | i <= 0    = pure ()
       | otherwise = do _ <- evaluate =<< act
                        go (i - 1)

makeTableUniform :: Int -> CondensedTable U.Vector Int
makeTableUniform n =
  tableFromProbabilities $ U.zip (U.enumFromN 0 n) (U.replicate n (1 / fromIntegral n))
{-# INLINE makeTableUniform #-}

main :: IO ()
main = do
  -- Set up tasty
  let tasty_opts  = [Option (Proxy :: Proxy Iterations)]
      ingredients = includingOptions tasty_opts : benchIngredients
  opts <- parseOptions ingredients (bench "Fake" (nf id ()))
  let iter = lookupOption opts
  -- Set up RNG
  mwc  <- create
  seed <- save mwc
  mtg  <- M.newMTGen . Just =<< uniform mwc
  defaultMainWithIngredients ingredients $ bgroup "All"
    [ bgroup "mwc"
      -- One letter group names are used so they will fit on the plot.
      --
      --  U - uniform
      --  R - uniformR
      --  D - distribution
      [ bgroup "U"
        [ bench "Double"  $ whnfIO $ loop iter (uniform mwc :: IO Double)
        , bench "Int"     $ whnfIO $ loop iter (uniform mwc :: IO Int)
        , bench "Int8"    $ whnfIO $ loop iter (uniform mwc :: IO Int8)
        , bench "Int16"   $ whnfIO $ loop iter (uniform mwc :: IO Int16)
        , bench "Int32"   $ whnfIO $ loop iter (uniform mwc :: IO Int32)
        , bench "Int64"   $ whnfIO $ loop iter (uniform mwc :: IO Int64)
        , bench "Word"    $ whnfIO $ loop iter (uniform mwc :: IO Word)
        , bench "Word8"   $ whnfIO $ loop iter (uniform mwc :: IO Word8)
        , bench "Word16"  $ whnfIO $ loop iter (uniform mwc :: IO Word16)
        , bench "Word32"  $ whnfIO $ loop iter (uniform mwc :: IO Word32)
        , bench "Word64"  $ whnfIO $ loop iter (uniform mwc :: IO Word64)
        ]
      , bgroup "R"
        -- I'm not entirely convinced that this is right way to test
        -- uniformR. /A.Khudyakov/
        [ bench "Double"  $ whnfIO $ loop iter (uniformR (-3.21,26) mwc :: IO Double)
        , bench "Int"     $ whnfIO $ loop iter (uniformR (-12,679)  mwc :: IO Int)
        , bench "Int8"    $ whnfIO $ loop iter (uniformR (-12,4)    mwc :: IO Int8)
        , bench "Int16"   $ whnfIO $ loop iter (uniformR (-12,679)  mwc :: IO Int16)
        , bench "Int32"   $ whnfIO $ loop iter (uniformR (-12,679)  mwc :: IO Int32)
        , bench "Int64"   $ whnfIO $ loop iter (uniformR (-12,679)  mwc :: IO Int64)
        , bench "Word"    $ whnfIO $ loop iter (uniformR (34,633)   mwc :: IO Word)
        , bench "Word8"   $ whnfIO $ loop iter (uniformR (34,63)    mwc :: IO Word8)
        , bench "Word16"  $ whnfIO $ loop iter (uniformR (34,633)   mwc :: IO Word16)
        , bench "Word32"  $ whnfIO $ loop iter (uniformR (34,633)   mwc :: IO Word32)
        , bench "Word64"  $ whnfIO $ loop iter (uniformR (34,633)   mwc :: IO Word64)
        ]
      , bgroup "D"
        [ bench "standard"    $ whnfIO $ loop iter (standard      mwc :: IO Double)
        , bench "normal"      $ whnfIO $ loop iter (normal 1 3    mwc :: IO Double)
        , bench "exponential" $ whnfIO $ loop iter (exponential 3 mwc :: IO Double)
        , bench "gamma,a<1"   $ whnfIO $ loop iter (gamma 0.5 1   mwc :: IO Double)
        , bench "gamma,a>1"   $ whnfIO $ loop iter (gamma 2   1   mwc :: IO Double)
        , bench "beta"        $ whnfIO $ loop iter (beta  2   3   mwc :: IO Double)
        , bench "chiSquare"   $ whnfIO $ loop iter (chiSquare 4   mwc :: IO Double)
          -- NOTE: We switch between algorithms when Np=10
        , bgroup "binomial"
          [ bench (show p ++ " " ++ show n) $ whnfIO $ loop iter (binomial n p mwc)
          | (n,p) <- [ (2,  0.2), (2,  0.5), (2,  0.8)
                     , (10, 0.1), (10, 0.9)
                     , (20, 0.2), (20, 0.8)
                       --
                     , (60,   0.2), (60,   0.8)
                     , (600,  0.2), (600,  0.8)
                     , (6000, 0.2), (6000, 0.8)
                     ]
          ]
        ]
        -- Test sampling performance. Table creation must be floated out!
      , bgroup "CT/gen" $ concat
        [ [ bench ("uniform "++show i) $ whnfIO $ loop iter (genFromTable tbl mwc)
          | i <- [2..10]
          , let tbl = makeTableUniform i
          ]
        , [ bench ("poisson " ++ show l) $ whnfIO $ loop iter (genFromTable tbl mwc)
          | l <- [0.01, 0.2, 0.8, 1.3, 2.4, 8, 12, 100, 1000]
          , let tbl = tablePoisson l
          ]
        , [ bench ("binomial " ++ show p ++ " " ++ show n) $ whnfIO $ loop iter (genFromTable tbl mwc)
          | (n,p) <- [ (4, 0.5), (10,0.1), (10,0.6), (10, 0.8), (100,0.4)]
          , let tbl = tableBinomial n p
          ]
        ]
        -- Benchmarking of setting up table (no need to use iterations
        -- here!). Setting up is rather expensive
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
        bench "Double" $ whnfIO $ loop iter (R.randomIO :: IO Double)
      , bench "Int"    $ whnfIO $ loop iter (R.randomIO :: IO Int)
      ]
    , bgroup "mersenne"
      [
        bench "Double" $ whnfIO $ loop iter (M.random mtg :: IO Double)
      , bench "Int"    $ whnfIO $ loop iter (M.random mtg :: IO Int)
      ]
#if MIN_VERSION_random(1,3,0)
    , bgroup "seed"
      [ bench "SeedGen.fromSeed" $ let rseed = R.toSeed seed :: R.Seed Seed
                                   in whnf R.fromSeed rseed
      , bench "SeedGen.toSeed"   $ whnf R.toSeed seed
      ]
#endif
    ]
