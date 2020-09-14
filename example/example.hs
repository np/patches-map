{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Control.Lens hiding (elements)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Patch.Class (Replace, Patched, ConflictResolution)
import Data.Map.Strict.Patch (PatchMap, act, transformWith)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Util (patchesFrom')
import Control.Concurrent
import Control.Concurrent.MVar
import Test.QuickCheck

newtype Term = Term Text deriving (Eq, Ord, Show)
newtype MultiTerm = MultiTerm [Term] deriving (Eq, Ord, Show)
type TermForms = Set MultiTerm
data TermCat = STOP | GRAPH | CANDIDATE deriving (Eq, Show)
type UserID = Int
type TermCats = Map UserID TermCat
type Conflicts = Int
type Label = MultiTerm

data TermInfo = TermInfo
  { _ti_cats      :: TermCats
  , _ti_forms     :: TermForms
  , _ti_conflicts :: Conflicts
  }
  deriving (Eq, Show)

makeLenses ''TermInfo

type Patch k v = PatchMap k (Replace (Maybe v))

type TermList = Map Label TermInfo

type TermListPatch = Patch Label TermInfo

type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }

data ClientData = ClientData
  { _c_name :: Text
  , _c_uid  :: UserID
  }

data API k v = API
  { _get :: IO (Versioned (Map k v))
  , _put :: Versioned (Patch k v) -> IO (Versioned (Patch k v))
  }

data Repo k v = Repo
  { _r_version :: Version
  , _r_map     :: Map k v
  , _r_history :: [Patch k v]
    -- ^ first patch in the list is the most recent
  }

-- Left biased
conflict :: ConflictResolution (Patch k TermInfo)
conflict _ Nothing  Nothing  = error "conflict: should not happen"
conflict _ Nothing  (Just y) = Just $ y & ti_conflicts +~ 1
conflict _ (Just x) Nothing  = Just $ x & ti_conflicts +~ 1
conflict _ (Just x) (Just y) = Just $ TermInfo
  { _ti_cats  = Map.union (_ti_cats x) (_ti_cats y)
  , _ti_forms = Set.union (_ti_forms x) (_ti_forms y)
  , _ti_conflicts = _ti_conflicts x + _ti_conflicts y + 1
  }

serve :: IO (API Label TermInfo)
serve = do
  v_repo <- newMVar (Repo 0 Map.empty [])
  pure $ API
    { _get = (\r -> Versioned (_r_version r) (_r_map r)) <$> readMVar v_repo
    , _put = \(Versioned v p) -> modifyMVar v_repo $ \r -> do
       let
         q = mconcat $ take (_r_version r - v) (_r_history r)
         (p', q') = transformWith conflict p q
         v' = _r_version r + 1
         r' = Repo
           { _r_version = v'
           , _r_map     = act p' $ _r_map r
           , _r_history = p' : _r_history r
           }
       pure (r', Versioned v' q')
    }

instance Arbitrary Term where
  arbitrary = Term <$> elements ["A", "B"]

instance Arbitrary MultiTerm where
  arbitrary = MultiTerm <$> ((:) <$> arbitrary <*> arbitrary)

instance Arbitrary TermInfo where
  arbitrary = TermInfo Map.empty -- TODO empty cats
                   <$> arbitrary
                   <*> pure 0

arbClient :: API Label TermInfo -> ClientData -> IO ()
arbClient api (ClientData name uid) = do
  Versioned v ts <- _get api
  go v ts
  where
    log :: Show d => Text -> d -> IO ()
    log msg d =
      T.putStrLn $ T.unwords [name, msg, T.pack $ show d]
    go v ts0 = do
      log "version/old" v
      log "termlist/old" ts0
      patch <- generate . scale (`div` 10) $ patchesFrom' ts0
      log "patch/cur" patch
      let ts1 = act patch ts0
      log "termlist/cur" ts1
      Versioned v' patch' <- _put api (Versioned v patch)
      log "version/new" v'
      log "patch/new" patch'
      let ts2 = act patch' ts1
      log "termlist/new" ts2
      delay <- generate $ elements [1..3]
      log "delay" delay
      threadDelay (1000000 * delay)
      go v' ts2

main :: IO ()
main = do
  api <- serve
  -- TODO if one client dies the whole program dies
  void . forkIO . arbClient api $ ClientData "Alice" 0
  arbClient api $ ClientData "Bob" 1
