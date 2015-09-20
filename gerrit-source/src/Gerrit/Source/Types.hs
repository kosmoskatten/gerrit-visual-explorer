-- | Module which declare "raw" data fetched directly from a Gerrit
-- server.
module Gerrit.Source.Types
    ( GerritCommitInfo (..)
    , GerritFileInfo (..)
    , GerritCommitFilter
    , GerritCommitEntry
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)

-- | General commit info for a Gerrit commit.
data GerritCommitInfo = 
    GerritCommitInfo { commitId      :: !Text
                     -- ^ The selected commit id which is change_id 
                     -- from Gerrit.
                     , commitSubject :: !Text
                     -- ^ The commit's subject.
                     , created       :: !UTCTime
                     , updated       :: !UTCTime
                     -- ^ The timestamp for the last update. This 
                     -- will be used.
                     , totInsertions :: !Int
                     -- ^ Total insertions for commit.
                     , totDeletions  :: !Int 
                     -- ^ Total deletions for commit.
                     }
    deriving Show

-- | Specific info about a file in a Gerrit commit.
data GerritFileInfo =
    GerritFileInfo { filePath      :: !Text
                   -- ^ Full path of the commited file.
                   , linesDeleted  :: !Int
                   -- ^ Lines deleted.
                   , linesInserted :: !Int 
                   -- ^ Lines inserted.
                   }
    deriving Show

-- | A filter function which can select a subset of the commits.
type GerritCommitFilter = ([GerritCommitInfo] -> IO [GerritCommitInfo])

-- | A commit entry with the commit info and a list of files.
type GerritCommitEntry = (GerritCommitInfo, [GerritFileInfo])
