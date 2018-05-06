import Control.Monad
import Data.Word
import qualified Data.Vector.Unboxed as U

import Test.HUnit hiding (Test)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework       (defaultMain)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import System.Random.MWC


----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = withSystemRandom $ \g -> defaultMain
  [ testProperty "save/restore"      $ prop_SeedSaveRestore g
  , testCase     "user save/restore" $ saveRestoreUserSeed
  , testCase     "empty seed data"   $ emptySeed
  ]

updateGenState :: GenIO -> IO ()
updateGenState g = replicateM_ 256 (uniform g :: IO Word32)


prop_SeedSaveRestore :: GenIO -> Property
prop_SeedSaveRestore g = monadicIO  $ do
  run $ updateGenState g
  seed  <- run $ save g
  seed' <- run $ save =<< restore seed
  return $ seed == seed'

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
