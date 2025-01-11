{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Word
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MVU
import Numeric.SpecFunctions           (logChoose,incompleteGamma,log1p)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.HUnit
import Test.QuickCheck.Monadic

import System.Random.MWC
import System.Random.MWC.Distributions
import System.Random.Stateful (StatefulGen)
#if MIN_VERSION_random(1,3,0)
import qualified System.Random.Stateful as Random (SeedGen(..))
#endif

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Average number of events per bin for binned statistical tests
newtype NPerBin = NPerBin Int

instance IsOption NPerBin where
  defaultValue = NPerBin 100
  parseValue = fmap NPerBin . safeRead
  optionName = pure "n-per-bin"
  optionHelp = pure "Average number of events per bin"


-- | P-value for statistical test.
newtype PValue = PValue Double

instance IsOption PValue where
  defaultValue = PValue 1e-9
  parseValue = fmap PValue . safeRead
  optionName = pure "pvalue"
  optionHelp = pure "P-value for statistical test"

----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = do
  -- Set up tasty
  let tasty_opts  = [ Option (Proxy :: Proxy NPerBin)
                    , Option (Proxy :: Proxy PValue)
                    , Option (Proxy :: Proxy QuickCheckTests)
                    , Option (Proxy :: Proxy QuickCheckReplay)
                    , Option (Proxy :: Proxy QuickCheckShowReplay)
                    , Option (Proxy :: Proxy QuickCheckMaxSize)
                    , Option (Proxy :: Proxy QuickCheckMaxRatio)
                    , Option (Proxy :: Proxy QuickCheckVerbose)
                    , Option (Proxy :: Proxy QuickCheckMaxShrinks)
                    ]
      ingredients = includingOptions tasty_opts : defaultIngredients
  opts <- parseOptions ingredients (testCase "Fake" (pure ()))
  let n_per_bin = lookupOption opts :: NPerBin
      p_val     = lookupOption opts
  --
  g0 <- createSystemRandom
  defaultMainWithIngredients ingredients $ testGroup "mwc"
    [ testProperty "save/restore"      $ prop_SeedSaveRestore g0
#if MIN_VERSION_random(1,3,0)
    , testProperty "SeedGen"           $ prop_SeedGen g0
#endif
    , testCase     "user save/restore" $ saveRestoreUserSeed
    , testCase     "empty seed data"   $ emptySeed
    , testCase     "output correct"    $ do
        g  <- create
        xs <- replicateM 513 (uniform g)
        assertEqual "[Word32]" xs golden
    , testCase "beta binomial mean"   $ prop_betaBinomialMean
    , testProperty "binomial is binomial" $ prop_binomial_PMF n_per_bin p_val g0
    ]

updateGenState :: GenIO -> IO ()
updateGenState g = replicateM_ 250 (uniform g :: IO Word32)

prop_SeedSaveRestore :: GenIO -> Property
prop_SeedSaveRestore g = monadicIO  $ do
  run $ updateGenState g
  seed  <- run $ save g
  seed' <- run $ save =<< restore seed
  return $ seed == seed'

#if MIN_VERSION_random(1,3,0)
prop_SeedGen :: GenIO -> Property
prop_SeedGen g = monadicIO $ do
  run $ updateGenState g
  seed <- run $ save g
  return $ seed == (Random.fromSeed . Random.toSeed) seed
#endif

saveRestoreUserSeed :: IO ()
saveRestoreUserSeed = do
  let seed = toSeed $ U.replicate 258 0
  seed' <- save =<< restore seed
  assertEqual "Seeds must be equal" seed' seed

emptySeed :: IO ()
emptySeed = do
  let seed = toSeed U.empty
  seed' <- save =<< create
  assertEqual "Seeds must be equal" seed' seed

-- First 513 values generated from seed made using create
golden :: [Word32]
golden =
  [ 2254043345, 562229898, 1034503294, 2470032534, 2831944869, 3042560015, 838672965, 715056843
  , 3122641307, 2300516242, 4079538318, 3722020688, 98524204, 1450170923, 2669500465, 2890402829
  , 114212910, 1914313000, 2389251496, 116282477, 1771812561, 1606473512, 1086420617, 3652430775
  , 1165083752, 3599954795, 3006722175, 341614641, 3000394300, 1378097585, 1551512487, 81211762
  , 604209599, 3949866361, 77745071, 3170410267, 752447516, 1213023833, 1624321744, 3251868348
  , 1584957570, 2296897736, 3305840056, 1158966242, 2458014362, 1919777052, 3203159823, 3230279656
  , 755741068, 3005087942, 2478156967, 410224731, 1196248614, 3302310440, 3295868805, 108051054
  , 1010042411, 2725695484, 2201528637, 667561409, 79601486, 50029770, 566202616, 3217300833
  , 2162817014, 925506837, 1527015413, 3079491438, 927252446, 118306579, 499811870, 2973454232
  , 2979271640, 4078978924, 1864075883, 197741457, 296365782, 1784247291, 236572186, 464208268
  , 1769568958, 827682258, 4247376295, 2959098022, 1183860331, 2475064236, 3952901213, 1953014945
  , 393081236, 1616500498, 2201176136, 1663813362, 2167124739, 630903810, 113470040, 924745892
  , 1081531735, 4039388931, 4118728223, 107819176, 2212875141, 1941653033, 3660517172, 192973521
  , 3653156164, 1878601439, 3028195526, 2545631291, 3882334975, 456082861, 2775938704, 3813508885
  , 1758481462, 3332769695, 3595846251, 3745876981, 152488869, 2555728588, 3058747945, 39382408
  , 520595021, 2185388418, 3502636573, 2650173199, 1077668433, 3548643646, 71562049, 2726649517
  , 494210825, 1208915815, 620990806, 2877290965, 3253243521, 804166732, 2481889113, 623399529
  , 44880343, 183645859, 3283683418, 2214754452, 419328482, 4224066437, 1102669380, 1997964721
  , 2437245376, 985749802, 858381069, 116806511, 1771295365, 97352549, 341972923, 2971905841
  , 110707773, 950500868, 1237119233, 691764764, 896381812, 1528998276, 1269357470, 2567094423
  , 52141189, 2722993417, 80628658, 3919817965, 3615946076, 899371181, 46940285, 4010779728
  , 318101834, 30736609, 3577200709, 971882724, 1478800972, 3769640027, 3706909300, 3300631811
  , 4057825972, 4285058790, 2329759553, 2967563409, 4080096760, 2762613004, 2518395275, 295718526
  , 598435593, 2385852565, 2608425408, 604857293, 2246982455, 919156819, 1721573814, 2502545603
  , 643962859, 587823425, 3508582012, 1777595823, 4119929334, 2833342174, 414044876, 2469473258
  , 289159600, 3715175415, 966867024, 788102818, 3197534326, 3571396978, 3508903890, 570753009
  , 4273926277, 3301521986, 1411959102, 2766249515, 4071012597, 959442028, 1962463990, 1098904190
  , 714719899, 562204808, 1658783410, 1471669042, 2565780129, 1616648894, 4236521717, 1788863789
  , 3068674883, 191936470, 253084644, 1915647866, 276372665, 2117183118, 3704675319, 218791054
  , 3680045802, 406662689, 3844864229, 91140313, 3834015630, 25116147, 904830493, 3152559113
  , 820358622, 1301896358, 296152699, 2202014455, 4256659428, 1175171414, 3287520873, 2028006499
  , 327448717, 2095642873, 3798661296, 58567008, 3907537112, 3691259011, 1730142328, 2373011713
  , 3387040741, 3189417655, 2949233059, 1238379614, 1813238023, 1064726446, 1339055235, 1744523609
  , 279811576, 2934103599, 283542302, 994488448, 418691747, 1062780152, 102211875, 4071713296
  , 1790834038, 1035092527, 2374272359, 3558280982, 1927663822, 3645417844, 3481790745, 3566282546
  , 2000290859, 505518126, 363501589, 4075468679, 3247300709, 3705242654, 2731103609, 2836871038
  , 589640144, 2546495106, 84767518, 1376911639, 2400770705, 527489676, 3804134352, 150084021
  , 240070593, 3807594859, 3518576690, 659503830, 2239678479, 1273668921, 4271050554, 3090482972
  , 401956859, 1772128561, 4438455, 1989666158, 2521484677, 3960178700, 4220196277, 1033999035
  , 2214785840, 3428469341, 428564336, 2517446784, 3935757188, 3294001677, 1037971963, 3590324170
  , 1220969729, 1719719817, 807688972, 77076422, 4251553858, 3963852375, 326128795, 3277818295
  , 3671513069, 549617771, 1683950556, 3352913781, 409318429, 2456264774, 4036950639, 1162718475
  , 83888874, 5578966, 172866494, 1542278848, 455546979, 1296511553, 4263636440, 2450589064
  , 372411483, 211216338, 2632256495, 2393754408, 1336054289, 4087203071, 3159642437, 1933346856
  , 2914152714, 3805541979, 2769740793, 1161287028, 2289749561, 4124509890, 2128452935, 210531695
  , 4250709834, 390950534, 1421430300, 3030519715, 3228987297, 3086837053, 2866915453, 2335948692
  , 1684378991, 2575634059, 4153427304, 2426048796, 4197556954, 2605152326, 2909410733, 2424889219
  , 654577921, 811955499, 118126602, 504071559, 1278756230, 3896458168, 4105558075, 750276169
  , 1120805572, 1762689330, 993728154, 1104363215, 774344996, 4077568952, 2183487324, 994724370
  , 3323036885, 3880704963, 746305447, 961608310, 2030117337, 453935768, 800490463, 1034636
  , 2323633564, 602565693, 806061242, 1899269713, 162686347, 467541008, 1529175313, 282891502
  , 2529616339, 2930657178, 464272784, 2878535316, 807165854, 3209080518, 4080120278, 347748171
  , 3972126063, 284174728, 2498328933, 1723872460, 143845955, 4223866687, 1761495357, 1544646770
  , 4206103283, 3771574626, 642165282, 1119501013, 3514063332, 1443320304, 4056369796, 3602131475
  , 1422908288, 804093687, 431176780, 40108717, 2998264213, 3705835674, 169805085, 454593842
  , 2781536994, 2385225212, 4137367775, 2631435125, 2347082354, 629238010, 3283635219, 3815791831
  , 1340400558, 4061846985, 3803921868, 3196119096, 718610843, 3694290834, 2169960411, 2407155570
  , 2557480499, 16164105, 480957288, 2155919829, 2490067282, 2356287132, 511737296, 1602800634
  , 1802275249, 3316832299, 50286484, 2106622541, 2352302834, 2538374315, 344766394, 2777260569
  , 1215135803, 2229011963, 114632277, 1645499402, 1111617833, 3833259754, 928611385, 686744723
  , 1898396834, 2461932251, 2665457318, 3797019621, 868313114, 2366635205, 481934875, 1170532970
  , 642610859, 3150733309, 3508548582, 666714469, 711663449, 2436617656, 2681476315, 1637296693
  , 2487349478, 4174144946, 2793869557, 559398604, 1898140528, 991962870, 864792875, 3861665129
  , 4024051364, 3383200293, 773730975, 33517291, 2660126073, 689133464, 2248134097, 3874737781
  , 3358012678]

-- We can test two for the price of one
betaBinomial :: StatefulGen g m =>
                Double -> Double -> Int -> g -> m Int
betaBinomial a b n g = do
  p <- beta a b g
  binomial n p g

nSamples :: Int
nSamples = 10000

alpha, delta :: Double
alpha = 600.0
delta = 400.0

nTrials :: Int
nTrials = 10

prop_betaBinomialMean :: IO ()
prop_betaBinomialMean = do
  g <- create
  ss <- replicateM nSamples $ betaBinomial alpha delta nTrials g
  let m = fromIntegral (sum ss) / fromIntegral nSamples
  let x1 = fromIntegral nTrials * alpha / (alpha + delta)
  assertBool ("Mean is " ++ show x1 ++ " but estimated as " ++ show m) (abs (m - x1) < 0.001)




-- Test that `binomial` really samples from binomial distribution.
--
-- If we have binomial random variate with number of trials N and
-- sample it M times. Then number of events with K successes is
-- described by multinomial distribution and we can test whether
-- experimental distribution is described using likelihood ratio test
prop_binomial_PMF :: NPerBin -> PValue -> GenIO -> Property
prop_binomial_PMF (NPerBin n_per_bin) (PValue p_val) g = property $ do
  p       <- choose (0, 1.0) -- Success probability
  n_trial <- choose (2, 100) -- Number of trials in binomial distribution
  -- Number of binomial samples to generate
  let n_samples  = n_trial * n_per_bin
      n_samples' = fromIntegral n_samples
  -- Compute number of outcomes
  pure $ ioProperty $ do
    hist <- do
      buf <- MVU.new (n_trial + 1)
      replicateM_ n_samples $
        MVU.modify buf (+(1::Int)) =<< binomial n_trial p g
      U.unsafeFreeze buf
    -- Here we compute twice log of likelihood ratio. Alternative
    -- hypothesis is some distribution which fits data perfectly
    --
    -- Asymtotically it's ditributed as χ² with n_trial-1 degrees of
    -- freedom
    let likelihood _ 0
          = 0
        likelihood k (fromIntegral -> n_obs)
          = n_obs * (log (n_obs / n_samples') - logProbBinomial n_trial p k)
    let logL = 2 * U.sum (U.imap likelihood hist)
    let significance = 1 - cumulativeChi2 (n_trial - 1) logL
    pure $ counterexample ("p     = " ++ show p)
         $ counterexample ("N     = " ++ show n_trial)
         $ counterexample ("p-val = " ++ show significance)
         $ counterexample ("chi2  = " ++ show logL)
         $ significance > p_val


----------------------------------------------------------------
-- Statistical helpers
----------------------------------------------------------------

-- Logarithm of probability for binomial distribution
logProbBinomial :: Int -> Double -> Int -> Double
logProbBinomial n p k
  = logChoose n k + log p * k' + log1p (-p) * nk'
  where
    k'  = fromIntegral k
    nk' = fromIntegral $ n - k

    
cumulativeChi2 :: Int -> Double -> Double
cumulativeChi2 (fromIntegral -> ndf) x
  | x <= 0    = 0
  | otherwise = incompleteGamma (ndf/2) (x/2)
