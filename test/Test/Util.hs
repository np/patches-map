{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util where

import           Control.Applicative
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Data.Patch.Class (Replace, replace)
import           Data.Map.Strict.Patch
import           Data.Validity

type Patch' = PatchMap Int (Replace (Maybe Int))
type Doc = Map Int Int

(*?) :: Num a => Maybe a -> Maybe a -> Maybe a
Nothing *? x       = x
x       *? Nothing = x
Just x  *? Just y  = Just (x * y)

nonEmpty :: Map k v -> Bool
nonEmpty = not . Map.null

type Edit k v = (k, Replace (Maybe v))

editsTo :: (Ord k, Arbitrary k, Eq v, Arbitrary v) => Map k v -> Gen (Edit k v)
editsTo m = gen

  where
    ks = Map.keys m
    gen = do
      k <- arbKey
      mv <- arbitrary
      return (k, replace (m !? k) mv)
    arbKey
      | null ks   = arbitrary
      | otherwise = do
          -- not sure if this is the right probability distribution...
          b <- arbitrary
          if b then elements ks
               else arbitrary

patchesFrom' :: (Show k, Ord k, Arbitrary k, Eq v, Arbitrary v)
             => Map k v -> Gen (PatchMap k (Replace (Maybe v)))
patchesFrom' m =
  fst <$> ((fromList <$> listOf (editsTo m)) `suchThat` (isValid . V . snd))

patchesFrom :: Doc -> Gen Patch'
patchesFrom = patchesFrom'

divergingPatchesFrom :: Doc -> Gen (Patch', Patch')
divergingPatchesFrom v = (,) <$> patchesFrom v <*> patchesFrom v

historyFrom :: Doc -> Int -> Gen [Patch']
historyFrom _ 0 = return []
historyFrom d m = do
  p <- patchesFrom d
  r <- historyFrom (act p d) $ m - 1
  return (p:r)

-- Workaround
newtype V = V { unV :: Validation }
instance Validity V where validate = unV

isV :: Validation -> Bool
isV = isValid . V
