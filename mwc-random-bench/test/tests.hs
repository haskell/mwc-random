import Test.Framework       (defaultMain)
import System.Random.MWC    (withSystemRandom)

import qualified MWC.QC
import qualified MWC.ChiSquare
import qualified MWC.KS


main :: IO ()
main = 
  withSystemRandom $ \g -> 
    defaultMain [ MWC.QC.tests        g
                , MWC.ChiSquare.tests g
                , MWC.KS.tests        g
                ]
