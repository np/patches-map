-- | For the implementation details, please see "Data.Patch.Internal"
module Data.Map.Strict.Patch
       (
         -- * Patches
         Patch (..)
       , toList
       , fromList
       , inverse
       , composable
         -- * Documents
       , apply
       , applicable
       , diff
         -- ** Transformations and merges
       , transformWith
         -- *** Conflict strategies
       , transform
       , ours
       , theirs
         -- * Edits
       , Edit
       , EditV
       , replace
       , replaceV
       , index
       , old
       , new
       , validEditV
       , validEdit
       )
       where

import Data.Map.Strict.Patch.Internal
