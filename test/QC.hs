-- QC tests for random number generators
--
-- Require QuickCheck >= 2.2

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic
import System.Random.MWC

import Data.Word (Word8,Word16,Word32,Word64,Word)
import Data.Int  (Int8, Int16, Int32, Int64 )



-- Ordered pair (x,y) for which x <= y
newtype OrderedPair a = OrderedPair (a,a)
                        deriving Show
instance (Ord a, Arbitrary a) => Arbitrary (OrderedPair a) where
  arbitrary = OrderedPair <$> suchThat arbitrary (\(x,y) -> x <= y)


----------------------------------------------------------------
-- Test that values generated with uniformR never lie outside range.

prop_InRange :: (Variate a, Ord a) => GenIO -> OrderedPair a -> Property
prop_InRange g (OrderedPair (x1,x2)) = monadicIO $ do
  r <- run $ uniformR (x1,x2) g
  assert (x1 <= r && r <= x2)

type InRange a = OrderedPair a -> Property

test_InRange :: IO ()
test_InRange = do
  g <- create
  let q :: (Testable prop) => prop -> IO ()
      q = quickCheck
  putStrLn "Int8"   >> q (prop_InRange g :: InRange Int8)
  putStrLn "Int16"  >> q (prop_InRange g :: InRange Int16)
  putStrLn "Int32"  >> q (prop_InRange g :: InRange Int32)
  putStrLn "Int64"  >> q (prop_InRange g :: InRange Int64)
  putStrLn "Word8"  >> q (prop_InRange g :: InRange Word8)
  putStrLn "Word16" >> q (prop_InRange g :: InRange Word16)
  putStrLn "Word32" >> q (prop_InRange g :: InRange Word32)
  putStrLn "Word64" >> q (prop_InRange g :: InRange Word64)
  putStrLn "Int"    >> q (prop_InRange g :: InRange Int)
  putStrLn "Word64" >> q (prop_InRange g :: InRange Word)
  putStrLn "Float"  >> q (prop_InRange g :: InRange Float)
  putStrLn "Double" >> q (prop_InRange g :: InRange Double)
