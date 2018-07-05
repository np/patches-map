{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util where

import           Control.Applicative
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Data.Map.Strict.Patch

type Patch' = Patch Int Int
type Doc = Map Int Int

(*?) :: Num a => Maybe a -> Maybe a -> Maybe a
Nothing *? x       = x
x       *? Nothing = x
Just x  *? Just y  = Just (x * y)

nonEmpty :: Map k v -> Bool
nonEmpty = not . Map.null

editsTo :: (Num k, Ord k, Arbitrary k, Eq v, Arbitrary v) => Map k v -> Gen (Edit k v)
editsTo m = gen `suchThatMap` id

  where
    ks = Map.keys m
    gen = do
      k <- arbKey
      mv <- arbitrary
      return $ replace k (m !? k) mv
    arbKey
      | null ks   = arbitrary
      | otherwise = do
          -- not sure if this is the right probability distribution...
          b <- arbitrary
          if b then elements ks
               else arbitrary

patchesFrom' :: (Num k, Ord k, Arbitrary k, Eq v, Arbitrary v) => Map k v -> Gen (Patch k v)
patchesFrom' m = fromList <$> listOf (editsTo m)

patchesFrom :: Doc -> Gen Patch'
patchesFrom = patchesFrom'

divergingPatchesFrom :: Doc -> Gen (Patch', Patch')
divergingPatchesFrom v = (,) <$> patchesFrom v <*> patchesFrom v

historyFrom :: Doc -> Int -> Gen [Patch']
historyFrom _ 0 = return []
historyFrom d m = do
  p <- patchesFrom d
  r <- historyFrom (apply p d) $ m - 1
  return (p:r)
