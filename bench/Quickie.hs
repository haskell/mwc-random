{-# LANGUAGE BangPatterns #-}
import System.Random.MWC (create, uniform)
import Control.Monad.ST (ST, runST)

u :: ST s Double
u = do
  let last = 1000000 :: Int
  gen <- create
  let loop !n !i | n == last = return i
                 | otherwise = uniform gen >>= loop (n+1)
  loop 0 0

main = print (runST u)
