{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
import qualified Data.Vector.Unboxed as U
import System.Random.Stateful
import System.Random.MWC
import System.Random.MWC.Distributions

import Data.List (sort, foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM, forM_)

data ADResult = ADResult
    { statistic  :: !Double    -- ^ A^2
    , pValue     :: !Double    -- ^ p-value
    , isAccepted :: !Bool     -- ^ H0 accepted?
    } deriving (Show, Eq)

data Distribution 
    = Normal 
    | Exponential !Double
    | Uniform !Double !Double
    | Logistic !Double !Double
    | Weibull !Double !Double
    | Cauchy !Double !Double
    | Gamma !Double !Double
    | Custom String (Double -> Double)
    -- deriving (Show) TODO

andersonDarlingTest :: Distribution -> [Double] -> Either String ADResult
andersonDarlingTest dist data_ = 
    case dist of
        Custom _ cdf -> andersonDarlingTestWithCDF cdf data_
        _            -> andersonDarlingTestWithCDF (distributionCDF dist) data_

andersonDarlingTestWithCDF :: (Double -> Double) -> [Double] -> Either String ADResult
andersonDarlingTestWithCDF _ [] = Left "Empty"
andersonDarlingTestWithCDF _ [x] = Left "Need at leas 2 values"
andersonDarlingTestWithCDF cdf xs
    | allEqual xs = Left "All values are equal"
    | otherwise = Right $ computeADTest cdf xs
  where
    allEqual ys = let (y:ys') = ys in all (== y) ys'

computeADTest :: (Double -> Double) -> [Double] -> ADResult
computeADTest cdf rawData = ADResult{..}
  where
    n          = length rawData
    n'         = fromIntegral n
    sortedData = sort rawData
    cdfValues  = map cdf sortedData
    -- Formula: A^2 = -n - (1/n) * sum((2i-1)*(ln(F(x_i)) + ln(1-F(x_{n+1-i}))))
    sumTerm    = sum
               $ zipWith3 calcTerm
                    [1..n]
                    cdfValues
                    (reverse cdfValues)
    calcTerm :: Int -> Double -> Double -> Double
    calcTerm i fi fRevI = weight * (log safeFi + log safeFRevI)
      where
        weight    = fromIntegral $ 2 * i - 1
        safeFi    = max fi            1e-15 -- prevent from `log 0.0`
        safeFRevI = max (1.0 - fRevI) 1e-15
    --
    aSquared = -n' - sumTerm / n'
    -- Correction for small samples
    aSquaredCorrected = case () of
        _ | n < 5     -> aSquared
          | otherwise -> aSquared -- * (1.0 + 0.75 / n' + 2.25 / (n' * n')) -- TODO
    statistic = aSquaredCorrected
    pValue = computePValue aSquaredCorrected
    isAccepted = pValue > 0.05 -- TODO

-- TODO bibliography
computePValue :: Double -> Double
computePValue a2
    | a2 < 0.2  = 1.0 - exp (-13.436 + 101.14 * a2 - 223.73 * a2 * a2)
    | a2 < 0.34 = 1.0 - exp (-8.318 + 42.796 * a2 - 59.938 * a2 * a2)
    | a2 < 0.6  = exp (0.9177 - 4.279 * a2 - 1.38 * a2 * a2)
    | a2 < 10.0 = exp (1.2937 - 5.709 * a2 + 0.0186 * a2 * a2)
    | otherwise = 0.0

distributionCDF :: Distribution -> (Double -> Double)
distributionCDF Normal = \x -> (1.0 + erf (x / sqrt 2.0)) / 2
  where
    erf x
        | x < 0 = -erf (-x)
        | otherwise =
            let a1 =  0.254829592
                a2 = -0.284496736
                a3 =  1.421413741
                a4 = -1.453152027
                a5 =  1.061405429
                p  =  0.3275911
                t  = 1.0 / (1.0 + p * x)
            in 1.0 - (a1*t  +  a2*t^2  +  a3*t^3  +  a4*t^4  +  a5*t^5) * exp (-x^2)
distributionCDF (Custom _ cdf) = cdf
distributionCDF _ = undefined -- TODO

-- | TODO url numpy
distributionCriticalValues :: Distribution -> [(Double, Double)]
distributionCriticalValues Normal = 
    [(0.15, 0.576), (0.10, 0.656), (0.05, 0.787), (0.025, 0.918), (0.01, 1.092)]
distributionCriticalValues _ = undefined

criticalValues :: [(Double, Double)]
criticalValues = distributionCriticalValues Normal


roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n)) / 10^n

showADResult :: ADResult -> String
showADResult (ADResult stat pval accepted) = 
    "Results:\n" ++
    "  A²: " ++ show (roundTo 4 stat) ++ "\n" ++
    "  p-value: " ++ show (roundTo 4 pval) ++ "\n" ++
    "  Accepted? " ++ show accepted ++ " (α = 0.05)"

mean :: Floating a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

stdDev :: Floating a => [a] -> a
stdDev xs =
    let n = fromIntegral (length xs)
        mu = mean xs
        variance = sum [ (x - mu) ^ 2 | x <- xs ] / (n - 1)
    in sqrt variance


main :: IO ()
main = do
  g <- initialize $ U.fromList [1,2,3]
  xs <- replicateM 1000000 $ normal 0 1 g -- 1_000_000
  -- print xs
  case andersonDarlingTest Normal xs of
        Left err -> putStrLn $ "Error: " ++ err
        Right r -> do
            putStrLn $ showADResult r
            writeFile ("digits.txt") (show xs)
            print $ sum xs
            putStrLn $ "mean   = " ++ show (mean xs)
            putStrLn $ "stddev = " ++ show (stdDev xs)
            print $ sum $ map abs xs
            print $ minimum xs
            print $ maximum xs
            -- print $ sort xs
            -- print $ map (distributionCDF Normal) (sort xs)
            -- putStrLn $ testWithCriticalValues Normal 0.05 r
  -- putStrLn "-------"
  -- forM_ [100, 1_000, 10_000, 100_000, 1_000_000] $ \ln -> do
  --   putStrLn $ "Size = " ++ show ln
  --   xs <- replicateM ln $ normal 0 1 g
  --   case andersonDarlingTest Normal xs of
  --     Left err -> putStrLn $ "Error: " ++ err
  --     Right (ADResult{..}) -> print statistic

