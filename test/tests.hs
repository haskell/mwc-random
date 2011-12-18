import Test.Framework       (defaultMain)
import System.Random.MWC    (withSystemRandom)

import qualified QC
import qualified Uniform



main :: IO ()
main = 
  withSystemRandom $ \g -> 
    defaultMain
    [ QC.tests      g
    , Uniform.tests g
    ]