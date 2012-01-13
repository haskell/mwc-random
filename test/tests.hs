import Test.Framework       (defaultMain)
import System.Random.MWC    (withSystemRandom)

import qualified QC
import qualified Uniform
import qualified KS


main :: IO ()
main = 
  withSystemRandom $ \g -> 
    defaultMain
    [ QC.tests      g
    , Uniform.tests g
    , KS.tests      g
    ]