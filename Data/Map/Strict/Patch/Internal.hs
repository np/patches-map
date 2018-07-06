{-# LANGUAGE BangPatterns #-}
-- | For day-to-day use, please see "Data.Map.Strict.Patch"
module Data.Map.Strict.Patch.Internal where
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.List as List
import Lens.Micro
import Control.Applicative
import Control.Monad
import Data.Function
import Control.Monad.ST
-- $setup
-- >>> import Test.QuickCheck
-- >>> import Test.Util
--
-- >>> :set -XScopedTypeVariables

-- $doctest_sucks
-- prop> forAll (patchesFrom d) $ \ x -> read (show x) == x

-- | A /patch/ is a collection of edits performed to a /document/, in this case a 'Map'. They are
--   implemented as a map of 'EditV'.
--   It can be converted to and from raw lists of edits using
--   'toList' and 'fromList' respectively.
--
--   Patches form a groupoid (a 'Monoid' with inverses, and a partial composition relation),
--   where the inverse element can be computed with 'inverse' and the groupoid operation
--   is /composition/ of patches. Applying @p1 <> p2@ is the same as applying @p1@ /then/
--   @p2@ (see 'apply'). This composition operator may produce structurally
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
-- prop> forAll (historyFrom d 3) $ \[a, b, c] -> apply (a <> (b <> c)) d == apply ((a <> b) <> c) d
--
-- The indices of the 'Edit' s of one 'Patch' are all based on the /original document/, so:
newtype Patch k v = Patch (Map k (EditV v)) deriving (Eq, Read, Show)

-- TODO doc
data EditV v = ReplaceV
    { _editV_old :: !(Maybe v)
    , _editV_new :: !(Maybe v)
    }
  deriving (Show, Read, Eq)

type Edit k v = (k, EditV v)
{-
-- | An 'Edit' is a single alteration of the vector, either inserting, removing, or replacing an element.
--
-- TODO Useful optics are provided below, for the 'index', the 'old' element, and the 'new' element.
data Edit k v = Replace k (Maybe v) (Maybe v) -- ^ @Replace k v v'@ replaces the element @v@ at position @k@ with @v'@.
  deriving (Show, Read, Eq)
-}

-- TODO doc
replaceV :: Eq v => Maybe v -> Maybe v -> Maybe (EditV v)
replaceV x y | x == y    = Nothing
             | otherwise = Just $ ReplaceV x y

-- TODO doc
replace :: Eq v => k -> Maybe v -> Maybe v -> Maybe (Edit k v)
replace k x y = (,) k <$> replaceV x y

-- | Compute the inverse of a patch, such that:
--
-- prop> forAll (patchesFrom d) $ \p -> p <> inverse p == mempty
--
-- prop> forAll (patchesFrom d) $ \p -> inverse p <> p == mempty
--
-- prop> forAll (patchesFrom d) $ \p -> inverse (inverse p) == p
--
-- prop> forAll (historyFrom d 2) $ \[p, q] -> inverse (p <> q) == inverse q <> inverse p
--
-- prop> forAll (patchesFrom d) $ \p -> inverse mempty == mempty
--
-- prop> forAll (patchesFrom d) $ \p -> applicable (inverse p) (apply p d)
--
-- prop> forAll (patchesFrom d) $ \p -> composable p (inverse p)
--
-- prop> forAll (patchesFrom d) $ \p -> composable (inverse p) p
inverse :: Patch k v -> Patch k v
inverse (Patch m) = Patch $ inverseEditV <$> m

inverseEditV :: EditV v -> EditV v
inverseEditV (ReplaceV ma mb) = ReplaceV mb ma

-- | A lens for the index where an edit is to be performed.
--
-- prop> nonEmpty d ==> forAll (editsTo d) $ \e -> set index v e ^. index == v
--
-- prop> nonEmpty d ==> forAll (editsTo d) $ \e -> set index (e ^. index) e == e
--
-- prop> nonEmpty d ==> forAll (editsTo d) $ \e -> set index v' (set index v e) == set index v' e
index :: Lens' (Edit k v) k
index = _1

-- | A traversal for the old element to be replaced/deleted. Empty in the case of an @Insert@.
old :: Traversal' (Edit k v) (Maybe v)
old = _2 . oldV

-- | A traversal for the old element to be replaced/deleted. Empty in the case of an @Insert@.
-- TODO: could lead to an ill formed `ReplaceV x x`
oldV :: Traversal' (EditV v) (Maybe v)
oldV f (ReplaceV o n) = (\o' -> ReplaceV o' n) <$> f o

-- | A traversal for the new value to be inserted or replacing the old value. Empty in the case of a @Delete@.
new :: Traversal' (Edit k v) (Maybe v)
new = _2 . newV

-- | A traversal for the new value to be inserted or replacing the old value. Empty in the case of a @Delete@.
-- TODO: could lead to an ill formed `ReplaceV x x`
newV :: Traversal' (EditV v) (Maybe v)
newV f (ReplaceV o n) = ReplaceV o <$> f n

-- | Convert a patch to a list of edits.
toList :: Patch k v -> [Edit k v]
toList (Patch m) = Map.toList m

-- | Checks that an edit is valid.
validEditV :: Eq v => EditV v -> Bool
validEditV (ReplaceV o n) = o /= n

-- | Checks that an edit is valid.
validEdit :: Eq v => Edit k v -> Bool
validEdit = validEditV . snd

-- | Convert a list of edits to a patch.
-- This keeps only one edit per key.
fromList :: (Ord k, Eq v) => [Edit k v] -> Patch k v
fromList = Patch . Map.fromList . filter validEdit

instance (Ord k, Eq v) => Monoid (Patch k v) where
  mempty = Patch Map.empty
  mappend (Patch p) (Patch q) =
      Patch $ Map.merge Map.preserveMissing Map.preserveMissing
                        (Map.zipWithMaybeMatched f) p q
    where
      f _ (ReplaceV o _) (ReplaceV _ o') = replaceV o o'

-- | Returns true if a patch can be safely applied to a document, that is,
--   @applicable p d@ holds when @d@ is a valid source document for the patch @p@.
applicable :: (Ord k, Eq v) => Patch k v -> Map k v -> Bool
applicable (Patch p) m = getAll $ Map.foldMapWithKey f p
  where
    f k (ReplaceV o _) = All (m !? k == o)

-- | Returns true if a patch can be validly composed with another.
--   That is, @composable p q@ holds if @q@ can be validly applied after @p@.
composable :: (Ord k, Eq v) => Patch k v -> Patch k v -> Bool
composable (Patch p) (Patch q) =
    getAll . fold $ Map.merge Map.dropMissing Map.dropMissing
                              (Map.zipWithMatched f) p q
  where
    f _ (ReplaceV _ o) (ReplaceV n _) = All (o == n)

-- | Apply a patch to a document.
--
-- Technically, 'apply' is a /monoid morphism/ to the monoid of endomorphisms @Map k v -> Map k v@,
-- and that's how we can derive the following two laws:
--
-- prop> forAll (historyFrom d 2) $ \[a, b] -> apply b (apply a d) == apply (a <> b) d
--
-- prop> apply mempty d == d
--
-- TODO: restore this version which does not require Show
-- apply :: (Ord k, Eq v) => Patch k v -> Map k v -> Map k v
apply :: (Show k, Show v, Ord k, Eq v) => Patch k v -> Map k v -> Map k v
apply p@(Patch em) im =
  Map.merge Map.preserveMissing (Map.mapMaybeMissing f)
            (Map.zipWithMaybeMatched g) im em
  where
    f _ (ReplaceV mo mn)
      | Nothing <- mo = mn
      | otherwise     = err "Nothing" mo
    g _ v (ReplaceV mo mn)
      | mo == Just v  = mn
      | otherwise     = err (show $ Just v) mo
    err expected found =
      error $ unlines
        [ "Data.Map.Patch.Internal.apply: invalid patch"
        , "Expected: " <> expected
        , "Found:    " <> show found
        , "Patch:    " <> show em
        , "Document: " <> show im
        ]

-- | Apply a patch to a document without checking that the patch is valid
-- for the given document.
unsafeApply :: (Ord k, Eq v) => Patch k v -> Map k v -> Map k v
unsafeApply p@(Patch em) im =
  Map.merge Map.preserveMissing (Map.mapMaybeMissing (const _editV_new))
            (Map.zipWithMaybeMatched (\_ _ -> _editV_new)) im em

-- | Given two diverging patches @p@ and @q@, @transformWith m p q@ returns
--   a pair of updated patches @(p',q')@ such that @q <> p'@ and
--   @p <> q'@ are equivalent patches that incorporate the changes
--   of /both/ @p@ and @q@, up to merge conflicts, which are handled by
--   the provided function @m@.
--
--   This is the standard @transform@ function of Operational Transformation
--   patch resolution techniques, and can be thought of as the pushout
--   of two diverging patches within the patch groupoid.
--
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith ours p q in apply (p <> q') d == apply (q <> p') d
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith ours p q in applicable p' (apply q d) && applicable q' (apply p d)
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith ours p q in composable p q' && composable q p'
--
--   This function is commutative iff @m@ is commutative.
--
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith (*?) p q; (q'', p'') = transformWith (*?) q p in p' == p'' && q' == q''
--
--   prop> forAll (patchesFrom d) $ \ p -> transformWith (*?) mempty p == (mempty, p)
--   prop> forAll (patchesFrom d) $ \ p -> transformWith (*?) p mempty == (p, mempty)
--   Some example conflict strategies are provided below.
transformWith :: (Ord k, Eq v) => (Maybe v -> Maybe v -> Maybe v)
              -> Patch k v -> Patch k v -> (Patch k v, Patch k v)
transformWith conflict p q = (p', q')
  where
    p' = transformFst conflict p q
    q' = transformFst (flip conflict) q p

-- | This is the internal version @transformWith@ which computes only the
--   first patch. The second patch can be obtained using @transformSnd@.
transformFst :: (Ord k, Eq v) => (Maybe v -> Maybe v -> Maybe v)
               -> Patch k v -> Patch k v -> Patch k v
transformFst conflict (Patch p) (Patch q)
    = Patch $ Map.merge Map.preserveMissing Map.dropMissing
                        (Map.zipWithMaybeMatched g) p q
  where
    g k x y | x == y = Nothing
    g k (ReplaceV _ nx) (ReplaceV _ ny) =
      replaceV ny (conflict nx ny)

-- | This is the internal version @transformWith@ which computes only the
--   second patch.
transformSnd :: (Ord k, Eq v) => (Maybe v -> Maybe v -> Maybe v)
               -> Patch k v -> Patch k v -> Patch k v
transformSnd = flip . transformFst . flip

-- | Resolve a conflict by always using the left-hand side
ours :: a -> a -> a
ours = const

-- | Resolve a conflict by always using the right-hand side
theirs :: a -> a -> a
theirs = flip const

-- | A convenience version of 'transformWith' which resolves conflicts using 'mappend'.
transform :: (Ord k, Eq v, Monoid v) => Patch k v -> Patch k v -> (Patch k v, Patch k v)
transform = transformWith (<>)

-- | Compute the difference between two maps.
--
-- prop> apply (diff d e) d == e
--
-- prop> diff d d == mempty
--
-- prop> apply (diff d e) d == apply (inverse (diff e d)) d
--
-- prop> apply (diff a b <> diff b c) a == apply (diff a c) a
--
-- prop> applicable (diff a b) a
diff :: (Ord k, Eq v) => Map k v -> Map k v -> Patch k v
diff v1 v2 = Patch $ d v1 v2
  where
    d = Map.merge (Map.mapMissing f) (Map.mapMissing g) (Map.zipWithMaybeMatched h)
    f _ o   = ReplaceV (Just o) Nothing
    g _   n = ReplaceV Nothing  (Just n)
    h _ o n = replaceV (Just o) (Just n)
