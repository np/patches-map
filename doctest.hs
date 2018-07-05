import Test.DocTest

main :: IO ()
main = doctest ["Data/Map/Strict/Patch/Internal.hs", "-i", "test", "-i."]
