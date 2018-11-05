
module Data.Map.Strict.Patch.InternalSpec where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck
import           Data.Patch.Class
import           Data.Validity

import           Data.Map.Strict (Map)
import           Data.Map.Strict.Patch.Internal
import           Test.Util (Patch', V(..))

spec :: Spec
spec = do
  describe "applicable" $ do
    it "mempty is always applicable" $ do
      property $ \d ->
        isValid (V $ applicable (mempty :: Patch') (d :: Map Int Int))
