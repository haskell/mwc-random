import Control.Monad.ST

import Data.Int
import Data.Word

import Criterion.Main
import System.Random.MWC

main = do
  gen <- create
  defaultMain 
    [ bench "mwc-Double" (uniform gen :: IO Double)
    , bench "mwc-Int"    (uniform gen :: IO Int   )
    , bench "mwc-Int8"   (uniform gen :: IO Int8  )
    , bench "mwc-Int16"  (uniform gen :: IO Int16 )
    , bench "mwc-Int32"  (uniform gen :: IO Int32 )
    , bench "mwc-Int64"  (uniform gen :: IO Int64 )
    , bench "mwc-Word"   (uniform gen :: IO Word  )
    , bench "mwc-Word8"  (uniform gen :: IO Word8 )
    , bench "mwc-Word16" (uniform gen :: IO Word16)
    , bench "mwc-Word32" (uniform gen :: IO Word32)
    , bench "mwc-Word64" (uniform gen :: IO Word64)
    , bench "mwc-Integer" (uniform gen :: IO Word64)
    , bench "normal"     (normal gen :: IO Double)
    ]
