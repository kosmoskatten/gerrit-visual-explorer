module Gerrit.Store.Types
    ( CommitStore (..)
    , CommitEntry (..)
    , Change (..)
    , File (..)
    , CommitSet
    , FileMap
    , FileHash
    , emptyStore
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Vector as Vector

-- | A timeline of commits.
type TimeLine = Vector CommitEntry

-- | A filename hash. Compact representation of a file.
type FileHash = Int

-- | A FileMap, where files are addressed using their hash values.
type FileMap = HashMap FileHash File

-- | A HashSet, to keep track of all stored commits without the need
-- to search the complete timeline.
type CommitSet = HashSet Text

-- | The commit store.
data CommitStore =
        CommitStore { timeLine  :: !TimeLine
                    -- ^ The main timeline of commits.
                    , fileMap   :: !FileMap
                    -- ^ The file map addressed using file hashes.
                    , commitSet :: !CommitSet
                    -- ^ The complete set of commits collected in a set
                    -- for fast lookup.
                    }
    deriving Show

-- | Entry representing one commit in the store.
data CommitEntry =
        CommitEntry { timeStamp :: !UTCTime
                    -- ^ The timestamp is the latest update time from
                    -- Gerrit.
                    , commitId  :: !Text
                    -- ^ The "long form" commit id including project and
                    -- branch details.
                    , subject   :: !Text
                    -- ^ The subject of the commit.
                    , changes   :: !(Vector Change)
                    -- ^ The sequence of changes included in this commit.
                    }
    deriving Show

-- | Representation of one change.
data Change = 
        Change { insertions :: {-# UNPACK #-} !Int
               -- ^ The number of insertions for the change.
               , deletions  :: {-# UNPACK #-} !Int
               -- ^ The number of deletions for the change.
               , fileRef    :: {-# UNPACK #-} !FileHash
               -- ^ The hash number reference to the file in the FileMap.
               }
    deriving Show

-- | Representation of one file.
data File =
        File { name   :: !Text
             -- ^ The file's name.
             , partOf :: !TimeLine
             -- ^ The timeline of the commits the file is part of.
             }
    deriving Show

-- | Make an empty store.
emptyStore :: CommitStore
emptyStore = CommitStore { timeLine  = Vector.empty
                         , fileMap   = HashMap.empty
                         , commitSet = HashSet.empty
                         }
