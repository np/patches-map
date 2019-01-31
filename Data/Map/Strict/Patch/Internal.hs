{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | For day-to-day use, please see "Data.Map.Strict.Patch"
module Data.Map.Strict.Patch.Internal where
import Data.Aeson
import Data.Group
import Data.Patch.Class
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Foldable
import Data.Monoid
import Data.Validity
import GHC.Generics (Generic)
-- $setup
-- >>> import Test.QuickCheck
-- >>> import Test.Util
--
-- >>> :set -XScopedTypeVariables

-- $doctest_sucks
-- prop> forAll (patchesFrom d) $ \ x -> read (show x) == x

-- | A /patch/ on a map is a map of patches.
--   Given a document of type @Map k v@, a patch is internally a
--   @Map k pv@ where @pv@ is the patch type for @v@.
--
--   If the values in the map are atomic @Replace@ can be used as a patch type.
--
--   A @PPatchatch@ can be converted to and from raw lists of value patches using
--   'toList' and 'fromList' respectively.
--
--   In general patches form a groupoid (a 'Monoid' with inverses, and a partial composition relation),
--   where the inverse element can be computed with 'invert' and the groupoid operation
--   is /composition/ of patches. Applying @p1 <> p2@ is the same as applying @p1@ /then/
--   @p2@ (see 'act'). This composition operator may produce structurally
--   different patches depending on associativity, however the patches are guaranteed to be /equivalent/
--   in the sense that the resultant document will be the same when they are applied.
--
--   For convenience, we make our composition operator here total, to fit the `Monoid` typeclass,
--   but provide some predicates ('composable' and 'applicable') to determine if the operation
--   can be validly used.
--
-- prop> forAll (patchesFrom d) $ \a -> a <> mempty == a
--
-- prop> forAll (patchesFrom d) $ \a -> mempty <> a == a
--
-- prop> forAll (historyFrom d 3) $ \[a, b, c] -> act (a <> (b <> c)) d == act ((a <> b) <> c) d
newtype PatchMap k pv = PatchMap (Map k pv) deriving (Eq, Read, Show, Generic, ToJSON)

instance (FromJSONKey k, Ord k, FromJSON pv, Monoid pv, Eq pv) =>
         FromJSON (PatchMap k pv) where
  parseJSON = fmap fromMap . parseJSON

newtype NotMempty a = NotMempty a

instance (Validity a, Monoid a, Eq a) => Validity (NotMempty a) where
  validate (NotMempty a) | a == mempty = invalid "mempty is invalid"
                         | otherwise   = validate a

instance (Validity pv, Monoid pv, Eq pv) => Validity (PatchMap k pv) where
  validate (PatchMap pm) =
    foldMap (delve "Data.Map.Strict.Patch: The sub parts of a patch-map" . NotMempty) pm

type instance ConflictResolution (PatchMap k pv) = k -> ConflictResolution pv

type instance Patched (PatchMap k pv) = Map k (Patched pv)

instance (Ord k, Eq v, Group v) => Group (PatchMap k v) where
  -- | Compute the inverse of a patch, such that:
  --
  -- prop> forAll (patchesFrom d) $ \p -> p <> invert p == mempty
  --
  -- prop> forAll (patchesFrom d) $ \p -> invert p <> p == mempty
  --
  -- prop> forAll (patchesFrom d) $ \p -> invert (invert p) == p
  --
  -- prop> forAll (historyFrom d 2) $ \[p, q] -> invert (p <> q) == invert q <> invert p
  --
  -- prop> forAll (patchesFrom d) $ \p -> invert mempty == mempty
  --
  -- prop> forAll (patchesFrom d) $ \p -> applicable (invert p) (act p d)
  --
  -- prop> forAll (patchesFrom d) $ \p -> composable p (invert p)
  --
  -- prop> forAll (patchesFrom d) $ \p -> composable (invert p) p
  invert (PatchMap m) = PatchMap $ invert <$> m

-- | Convert a patch to a list of edits.
toList :: PatchMap k pv -> [(k, pv)]
toList (PatchMap m) = Map.toList m

-- | Convert a map of edits to a patch.
-- Eq and Monoid are used to discard empty 'pv' patches.
fromMap :: (Monoid pv, Eq pv) => Map k pv -> PatchMap k pv
fromMap = PatchMap . Map.filter (/= mempty)

-- | Convert a list of edits to a patch.
-- This keeps only one edit per key.
-- Eq and Monoid are used to discard empty 'pv' patches.
-- Note that it wouldn't make sense to compose multiple value patches ('pv')
-- since all the given value patches are supposed to have a common domain.
fromList :: (Show k, Ord k, Validity pv, Monoid pv, Eq pv)
         => [(k, pv)] -> (PatchMap k pv, Validation)
fromList xs = (fromMap $ Map.fromList xs, v)
  where
    v = decorateList xs (validate . snd)
     <> mconcat [ invalid $ "Duplicate key: " <> show k | k <- dups ]
    dups = Map.keys . Map.filter (> Sum 1) $ Map.fromListWith (<>) [ (x, Sum 1) | (x, _) <- xs ]

singleton :: (Ord k, Validity pv, Monoid pv, Eq pv)
          => k -> pv -> (PatchMap k pv, Validation)
singleton k pv
  | pv == mempty = mempty
  | otherwise    = (PatchMap $ Map.singleton k pv, validate pv)

instance (Ord k, Eq pv, Monoid pv) => Semigroup (PatchMap k pv) where
  PatchMap p <> PatchMap q =
    PatchMap $ Map.merge Map.preserveMissing Map.preserveMissing
                      (zipWithNonMemptyMatched (const (<>))) p q

instance (Ord k, Eq pv, Monoid pv) => Monoid (PatchMap k pv) where
  mempty = PatchMap Map.empty

instance (Ord k, Composable pv, Eq pv) => Composable (PatchMap k pv) where
  -- | Returns true if a patch can be validly composed with another.
  --   That is, @composable p q@ holds if @q@ can be validly applied after @p@.
  composable (PatchMap p) (PatchMap q) = fold2 (const composable) p q

instance (Ord k, Applicable pv (Maybe v)) => Applicable (PatchMap k pv) (Map k v) where
  -- | Returns true if a patch can be safely applied to a document, that is,
  --   @applicable p d@ holds when @d@ is a valid source document for the patch @p@.
  applicable (PatchMap p) m = Map.foldMapWithKey f p
    where
      f k pv = applicable pv (m !? k)

instance (Ord k, Applicable pv (Maybe v))
      => Applicable (PatchMap k pv) (Maybe (Map k v)) where
  applicable (PatchMap p) Nothing =
    check (Map.null p) "PatchMap should be empty here"
  applicable p (Just m) = applicable p m

instance (Ord k, Action pv (Maybe v))
      => Action (PatchMap k pv) (Map k v) where
  -- | Apply a patch to a document without checking that the patch is valid
  -- for the given document.
  -- unsafeApply :: (Ord k, Eq v) => PatchMap k v -> Map k v -> Map k v
  act (PatchMap em) im =
    Map.merge Map.preserveMissing (Map.mapMaybeMissing (const (`act` Nothing)))
              (Map.zipWithMaybeMatched (\_ v pv -> pv `act` Just v)) im em

instance (Ord k, Action pv (Maybe v))
      => Action (PatchMap k pv) (Maybe (Map k v)) where
  act = fmap . act

instance (Ord k, Monoid pv, Eq pv, Transformable pv) => Transformable (PatchMap k pv) where
  -- | This is the internal version @transformWith@ which computes only the
  --   first patch. The second patch can be obtained using @transformSnd@.
  transformFst conflict (PatchMap p) (PatchMap q)
    = PatchMap $ Map.merge Map.preserveMissing Map.dropMissing
                          (zipWithNonMemptyMatched $ transformFst . conflict) p q

  -- | This is the internal version @transformWith@ which computes only the
  --   second patch.
  transformSnd conflict (PatchMap p) (PatchMap q)
    = PatchMap $ Map.merge Map.dropMissing Map.preserveMissing
                        (zipWithNonMemptyMatched $ transformSnd . conflict) p q

  transformable (PatchMap p) (PatchMap q) = fold2 (const transformable) p q

  conflicts (PatchMap p) (PatchMap q) = fold2 (const conflicts) p q

-- | Compute the difference between two maps.
--
-- prop> act (diff d e) d == e
--
-- prop> diff d d == mempty
--
-- prop> act (diff d e) d == act (invert (diff e d)) d
--
-- prop> act (diff a b <> diff b c) a == act (diff a c) a
--
-- prop> applicable (diff a b) a
diff :: (Ord k, Eq v) => Map k v -> Map k v -> PatchMap k (Replace (Maybe v))
diff v1 v2 = PatchMap $ d v1 v2
  where
    d = Map.merge (Map.mapMissing f) (Map.mapMissing g) (Map.zipWithMaybeMatched h)
    f _ o   = replace (Just o) Nothing
    g _   n = replace Nothing  (Just n)
    h _ o n = nonMempty $ replace (Just o) (Just n)

zipWithNonMemptyMatched :: (Applicative f, Monoid z, Eq z)
                        => (k -> x -> y -> z) -> Map.WhenMatched f k x y z
zipWithNonMemptyMatched f = Map.zipWithMaybeMatched g
  where
    g k x y = nonMempty $ f k x y

nonMempty :: (Monoid a, Eq a) => a -> Maybe a
nonMempty a | a == mempty = Nothing
            | otherwise   = Just a

fold2 :: (Ord k, Monoid m) => (k -> a -> b -> m) -> Map k a -> Map k b -> m
fold2 f m1 m2 = fold $ Map.merge Map.dropMissing Map.dropMissing
                                 (Map.zipWithMatched f) m1 m2
