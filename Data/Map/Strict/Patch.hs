-- | For the implementation details, please see "Data.Patch.Internal"
module Data.Map.Strict.Patch
       (
         -- * Patches
         Patch (..)
       , toList
       , fromList

         -- * Documents
       , diff

         -- * Re-exports

         -- ** Inverse and composition
       , invert
       , composable

         -- ** Apply patches
       , act
       , applicable

         -- ** Transformations and merges
       , transformWith
         -- *** Conflict strategies
       , transform
       , ours
       , theirs
         -- ** Replace
       , Replace
       , old
       , new
       )
       where

import Data.Patch.Class
import Data.Map.Strict.Patch.Internal
