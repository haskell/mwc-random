import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-fobject-code", "System/Random/MWC.hs"]
