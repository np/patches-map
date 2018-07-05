
module Data.Map.Strict.Patch.InternalSpec where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

import           Data.Map.Strict.Patch.Internal
import           Test.Util (Patch')

spec :: Spec
spec = do
  describe "applicable" $ do
    it "mempty is always applicable" $ do
      property $ \d ->
        applicable (mempty :: Patch') d
