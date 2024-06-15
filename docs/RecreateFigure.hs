{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main(main) where

import System.FilePath ()
import Data.Colour
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding ( sample, render, Vector, offset )
import System.Environment


majorizingFn :: Int -> Double -> Double -> Double
majorizingFn i pp x
  | x <= xL - 0.5 = c * exp (-lambdaL * (xL - x - 0.5))
  | x <= xR - 0.5 = (1 + c) - abs (bigM - x) / p1
  | otherwise     = c * exp (-lambdaR * (x + 0.5 - xR))
  where
    (xL, xR, _, c, p1, bigM, fm, q, r) = xLRMc i pp
    a = (fm - xL) / (fm - xL * r)
    lambdaL = a * (1 + a / 2)
    b = (xR - fm) / (xR * q)
    lambdaR = b * (1 + b / 2)

minorizingFn :: Int-> Double -> Double -> Double
minorizingFn i pp x
  | x <= xL - 0.5 = 0.0
  | x <= xR - 0.5 = 1.0 - abs (bigM - x) / p1
  | otherwise     = 0.0
  where
    (xL, xR, _, _, p1, bigM, _, _, _) = xLRMc i pp

xLRMc :: Int -> Double ->
         (Double, Double, Double, Double, Double, Double, Double, Double, Double)
xLRMc i pp = (xL, xR, xM, c, p1, bigM, fm, q, r)
  where
    m :: Double
    m = fromIntegral i
    r = min pp (1 - pp)
    q = 1 - r
    fm = m * r + r
    bigM :: Double
    bigM = fromIntegral $ floor fm
    p1 = (fromIntegral (floor (2.195 * sqrt (m * pp * q) - 4.6 * q) :: Int)) + 0.5
    xM = bigM + 0.5
    xL = xM - p1
    xR = xM + p1
    c  = 0.134 + 20.5 / (15.3 + bigM)

n :: Int
n = 200

p :: Double
p = 0.25

m1, sd :: Double
m1 = (fromIntegral n) * p + p
sd = sqrt $ (fromIntegral n) * p * (1- p)

lb, ub :: Double
lb = m1 - 3 * sd
ub = m1 + 3 * sd

tick, skala :: Double
tick = 0.01
skala = 20.0

majors :: [(Double, Double)]
majors = [(x, majorizingFn n p x) | x <- [lb, lb + tick .. ub]]

minors :: [(Double, Double)]
minors = [(x, minorizingFn n p x) | x <- [lb, lb + tick .. ub]]

integralBinomialPDF :: (Integral a, Fractional b) => a -> b -> a -> b
integralBinomialPDF t q 0 = (1 - q)^t
integralBinomialPDF t q x = integralBinomialPDF t q (x - 1) * a * b
  where
    a = fromIntegral (t - x + 1) / fromIntegral x
    b = q / (1 - q)

binPdfs :: [Double]
binPdfs = map (/ v) $ map (integralBinomialPDF n p) $ map floor $ map (+ 0.5) [lb :: Double, lb + tick .. ub]

v :: Double
v = integralBinomialPDF n p bigM
  where
    bigM = floor $ (fromIntegral n) * p + p

majorVs :: [P2 Double]
majorVs = map p2 majors

binPdfVs :: [P2 Double]
binPdfVs = map p2 $ zip (map fst majors) binPdfs

minorVs :: [P2 Double]
minorVs = map p2 minors

lineChart :: String -> IO ()
lineChart fn = do
  withArgs ["-h 800", "-w 800", "-o" ++ fn ++ ".svg"] (mainWith exampleQ)

main :: IO ()
main = lineChart "docs/RecreateFigure"

exampleQ :: Diagram B
exampleQ = strutX 2 ||| mconcat
  [ mconcat cs # fc purple # lw none

  -- x-axis
  , strokeLocT xAxisT  # lc black # lwL 0.01 # scaleY skala
  , moveTo (p2 (xM - offset , - 2.0)) $ text "Number of Successes"
  , strokeLocT brokenXAxisT # lc black # lwL 0.05

  -- y-axis
  , strokeLocT yAxisT  # lc black # lwL 0.01 # scaleY skala
  , moveTo (p2 (-sd - yAxixOffset - 3.0 , skala * 1.0)) $ text "Probability Density (Unnormalised)" # rotateBy (1/4)

  -- Key
  , let majKey = fromVertices $ map p2 [ (xM - offset - 10.0, skala * 2.0 - 2.0)
                                       , (xM - offset, skala * 2.0 - 2.0)
                                       ]
    in strokeLocT majKey # lc blue # lwL 0.1 ===
       moveTo (p2 (xM - offset + 8, skala * 2.0 - 4.0)) (text "Majorizing Function" # fc blue)
  , let minKey = fromVertices $ map p2 [ (xM - offset - 10.0, skala * 2.0 - 4.0)
                                       , (xM - offset, skala * 2.0 - 4.0)
                                       ]
    in strokeLocT minKey # lc green # lwL 0.1 ===
       moveTo (p2 (xM - offset + 8, skala * 2.0 - 6.0)) (text "Minorizing Function" # fc green)
  , let tgtKey = fromVertices $ map p2 [ (xM - offset - 10.0, skala * 2.0 - 6.0)
                                       , (xM - offset, skala * 2.0 - 6.0)
                                       ]
    in strokeLocT tgtKey # lc red # lwL 0.1 ===
       moveTo (p2 (xM - offset + 8, skala * 2.0 - 8.0)) (text "Target PDF" # fc red)
  , moveTo (p2 (xM - offset - 3 * sd, skala * 2.0 - 10.0)) (text ("n = " ++ show n)  # fc black)
  , moveTo (p2 (xM - offset - 3 * sd, skala * 2.0 - 11.0)) (text ("p = " ++ show p)  # fc black)

  -- Areas
  , area1, area2, area3, area4
  , moveTo (p2 (xM - 0.5 - offset, 0.5 * skala * xMinM)) $ text "One"
  , moveTo (p2 (xM - 0.5 - offset, skala * (0.5 * (xMajM - xMinM) + xMinM))) $ text "Two"
  , moveTo (p2 (lb + (xL - lb) / 2 - offset, 0.5 * skala * majorizingFn n p (lb + (xL - lb) / 2))) $ text "Three"
  , moveTo (p2 (ub + (xR - ub) / 2 - offset, 0.5 * skala * majorizingFn n p (ub + (xR - ub) / 2))) $ text "Four"

  , strokeLocT majorT  # lc blue  # lwL 0.01 # scaleY skala
  , strokeLocT binPdfT # lc red   # lwL 0.01 # scaleY skala
  , strokeLocT minorT  # lc green # lwL 0.01 # scaleY skala
  , strokeLocT verticalLT # lc purple # lwL 0.01
  , strokeLocT verticalRT # lc purple # lwL 0.01

  , moveTo (p2 (xL - 0.5 - offset, - 1.0)) $ text $ show (xL - 0.5)
  , moveTo (p2 (xR - 0.5 - offset, - 1.0)) $ text $ show (xR - 0.5)
  , moveTo (p2 (0.0, - 1.0)) $ text $ show $ (fromIntegral (round (lb * 100))) / 100
  , moveTo (p2 (ub - offset, - 1.0)) $ text $ show $ (fromIntegral (round (ub * 100))) / 100
  , let y = 1.5 in moveTo (p2 (-sd - yAxixOffset - 1.0, skala * y)) $ text $ show $ (fromIntegral (round (y * 100))) / 100
  , let y = 1.0 in moveTo (p2 (-sd - yAxixOffset - 1.0, skala * y)) $ text $ show $ (fromIntegral (round (y * 100))) / 100
  , let y = 0.5 in moveTo (p2 (-sd - yAxixOffset - 1.0, skala * y)) $ text $ show $ (fromIntegral (round (y * 100))) / 100
  ]
  where
    xAxisT :: Located (Trail V2 Double)
    xAxisT = (fromVertices $ map p2 [(m1 - 4 * sd, 0), (m1 + 4 * sd, 0)]) `at` (-sd ^& 0)

    brokenXAxisT :: Located (Trail V2 Double)
    brokenXAxisT = fromVertices $ map p2 [ (m1 - 4 * sd - offset - 0.0,  0.0)
                                         , (m1 - 4 * sd - offset - 0.1,  0.3)
                                         , (m1 - 4 * sd - offset - 0.2,  0.0)
                                         , (m1 - 4 * sd - offset - 0.3, -0.3)
                                         , (m1 - 4 * sd - offset - 0.4,  0.0)
                                         , (m1 - 4 * sd - offset - 2.0,  0.0)
                                         ]

    yAxixOffset = 2.0
    yAxisT :: Located (Trail V2 Double)
    yAxisT = fromVertices $ map p2 [(-sd - yAxixOffset , 0.0), (-sd - yAxixOffset , 2.0)]

    binPdfT :: Located (Trail V2 Double)
    binPdfT = fromVertices binPdfVs `at` (0 ^& (minimum binPdfs))

    majorT :: Located (Trail V2 Double)
    majorT = fromVertices majorVs `at` (0 ^& (minimum (map snd majors)))

    minorT :: Located (Trail V2 Double)
    minorT = fromVertices minorVs `at` (0 ^& (minimum (map snd minors)))

    verticalLT :: Located (Trail V2 Double)
    verticalLT = fromVertices $ map p2 [(xL - 0.5 - offset, 0.0)
                                      , (xL - 0.5 - offset, skala * xMajL)
                                      ]

    verticalRT :: Located (Trail V2 Double)
    verticalRT = fromVertices $ map p2 [ (xR - 0.5 - offset, 0.0)
                                       , (xR - 0.5 - offset, skala * xMajR)
                                       ]

    pt1 = (xL - 0.5, 0.0)
    pt2 = (xR - 0.5, 0.0)
    pt3  = (xM - 0.5, skala * xMinM)

    area1 :: Diagram B
    area1 = t # fc red # opacity 0.1
      where
        t :: Diagram B
        t = strokeLocT $ mapLoc Trail u
        u :: Located (Trail' Loop V2 Double)
        u = ((fromVertices $ map p2 [pt1, pt2, pt3]) # closeLine) `at`  ((xL - offset -0.5) ^& 0)

    area2 :: Diagram B
    area2 = t # fc blue # opacity 0.1
      where
        t :: Diagram B
        t = strokeLocT $ mapLoc Trail u
        u :: Located (Trail' Loop V2 Double)
        u = (fromVertices ([w1] ++ vs ++ [wn, w2]) # closeLine) `at` ((xL - offset - 0.5) ^& 0)
        vs = fmap (scaleY 20.0) $ filter (\x -> fst (unp2 x) <= xR - 0.5) $ filter (\x -> fst (unp2 x) > xL - 0.5) majorVs
        w1 = p2 (fst (unp2 (head vs)), 0.0)
        w2 = p2 (xM - 0.5, skala * xMinM)
        wn = p2 (fst (unp2 (last vs)), 0.0)

    area3 :: Diagram B
    area3 = t # fc yellow # opacity 0.1
      where
        t :: Diagram B
        t = strokeLocT $ mapLoc Trail u
        u :: Located (Trail' Loop V2 Double)
        u = (fromVertices ([w1] ++ vs ++ [wn]) # closeLine) `at` (0 ^& 0)
        vs = fmap (scaleY 20.0) $ filter (\x -> fst (unp2 x) <= xL - 0.5) majorVs
        w1 = p2 (fst (unp2 (head vs)), 0.0)
        wn = p2 (fst (unp2 (last vs)), 0.0)

    area4 :: Diagram B
    area4 = t # fc yellow # opacity 0.1
      where
        t :: Diagram B
        t = strokeLocT $ mapLoc Trail u
        u :: Located (Trail' Loop V2 Double)
        u = (fromVertices ([w1] ++ vs ++ [wn]) # closeLine) `at` ((xR - offset - 0.5) ^& 0)
        vs = fmap (scaleY 20.0) $ filter (\x -> fst (unp2 x) > xR - 0.5) majorVs
        w1 = p2 (fst (unp2 (head vs)), 0.0)
        wn = p2 (fst (unp2 (last vs)), 0.0)

    cs = map mkCircle $ map p2 $ [ (xL - 0.5 - offset, 0.0)
                                 , (xL - 0.5 - offset, skala * xMajL)
                                 , (xM - 0.5 - offset, skala * xMinM)
                                 , (xM - 0.5 - offset, skala * xMajM)
                                 , (xR - 0.5 - offset, 0.0)
                                 , (xR - 0.5 - offset, skala * xMajR)
                                 , (0.0, 0.0)
                                 , (ub - offset, 0.0)
                                 ]

    (xL, xR, xM, _, _, _, _, _, _) = xLRMc n p

    xMinM = minorizingFn n p (xM - 0.5)
    xMajL = majorizingFn n p (xL - 0.5)
    xMajR = majorizingFn n p (xR - 0.5)
    xMajM = majorizingFn n p (xM - 0.5)
    offset = minimum (map fst majors)

    mkCircle q = circle 0.1 # moveTo q
