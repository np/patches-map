{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | For day-to-day use, please see "Data.Map.Strict.Patch"
module Data.Map.Strict.Patch.Internal where
import Data.Group
import Data.Patch.Class
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Foldable
import Data.Monoid
import Data.Validity
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
--   A @Patch@ can be converted to and from raw lists of value patches using
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
newtype Patch k pv = Patch (Map k pv) deriving (Eq, Read, Show)

type instance ConflictResolution (Patch k pv) = k -> ConflictResolution pv

type instance Patched (Patch k pv) = Map k (Patched pv)

instance (Ord k, Eq v, Group v) => Group (Patch k v) where
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
  invert (Patch m) = Patch $ invert <$> m

-- | Convert a patch to a list of edits.
toList :: Patch k pv -> [(k, pv)]
toList (Patch m) = Map.toList m

-- | Convert a list of edits to a patch.
-- This keeps only one edit per key.
fromList :: (Ord k, Validity pv) => [(k, pv)] -> Patch k pv
fromList = Patch . Map.fromList . filter (isValid . snd)

instance (Ord k, Eq pv, Monoid pv) => Monoid (Patch k pv) where
  mempty = Patch Map.empty
  mappend (Patch p) (Patch q) =
    Patch $ Map.merge Map.preserveMissing Map.preserveMissing
                      (zipWithNonMemptyMatched (const mappend)) p q

instance (Ord k, Composable pv, Eq pv) => Composable (Patch k pv) where
  -- | Returns true if a patch can be validly composed with another.
  --   That is, @composable p q@ holds if @q@ can be validly applied after @p@.
  composable (Patch p) (Patch q) = fold2 (const composable) p q

instance (Ord k, Applicable pv (Maybe v)) => Applicable (Patch k pv) (Map k v) where
  -- | Returns true if a patch can be safely applied to a document, that is,
  --   @applicable p d@ holds when @d@ is a valid source document for the patch @p@.
  applicable (Patch p) m = Map.foldMapWithKey f p
    where
      f k pv = applicable pv (m !? k)

instance (Ord k, Action pv (Maybe v)) => Action (Patch k pv) (Map k v) where
  -- | Apply a patch to a document without checking that the patch is valid
  -- for the given document.
  -- unsafeApply :: (Ord k, Eq v) => Patch k v -> Map k v -> Map k v
  act (Patch em) im =
    Map.merge Map.preserveMissing (Map.mapMaybeMissing (const (`act` Nothing)))
              (Map.zipWithMaybeMatched (\_ v pv -> pv `act` Just v)) im em

instance (Ord k, Monoid pv, Eq pv, Transformable pv) => Transformable (Patch k pv) where
  -- | This is the internal version @transformWith@ which computes only the
  --   first patch. The second patch can be obtained using @transformSnd@.
  transformFst conflict (Patch p) (Patch q)
    = Patch $ Map.merge Map.preserveMissing Map.dropMissing
                          (zipWithNonMemptyMatched $ transformFst . conflict) p q

  -- | This is the internal version @transformWith@ which computes only the
  --   second patch.
  transformSnd conflict (Patch p) (Patch q)
    = Patch $ Map.merge Map.dropMissing Map.preserveMissing
                        (zipWithNonMemptyMatched $ transformSnd . conflict) p q

  transformable (Patch p) (Patch q) = getAll $ fold2 f p q
    where
      f _ pv qv = All (transformable pv qv)

  conflicts (Patch p) (Patch q) = getSum $ fold2 f p q
    where
      f _ pv qv = Sum (conflicts pv qv)

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
diff :: (Ord k, Eq v) => Map k v -> Map k v -> Patch k (Replace (Maybe v))
diff v1 v2 = Patch $ d v1 v2
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
