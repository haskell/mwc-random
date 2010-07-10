{-# LANGUAGE Rank2Types #-}
import Control.Monad.ST
import Criterion.Main
import System.Random.MWC

main = do
  gen <- create
  defaultMain [
        bench "mwc" (uniform gen :: IO Double)
        ]
