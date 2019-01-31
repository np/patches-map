import Criterion.Main
import Data.Map.Strict.Patch
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Debug.Trace
import Test.Util

qcgen :: QCGen
qcgen = mkQCGen 19835315

gen :: Int -> (Patch', Doc)
gen i = let
          doc = unGen arbitrary qcgen i :: Doc
          patch = unGen (patchesFrom doc) qcgen i
        in traceShow (length $ toList patch, length doc) (patch,doc)

main :: IO ()
main =
  defaultMain [ bgroup "apply" [ bench "1" $ nf (uncurry act) (gen 500)
--                             , bench "2" $ nf (uncurry act) (gen 1000)
--                             , bench "3" $ nf (uncurry act) (gen 2000)
--                             , bench "4" $ nf (uncurry act) (gen 4000)
                               ]
              ]
