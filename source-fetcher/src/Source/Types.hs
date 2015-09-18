-- | Module which declare "raw" data fetched directly from a Gerrit
-- server.
module Source.Types
    ( CommitInfo (..)
    , FileInfo (..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)

-- | General commit info.
data CommitInfo = 
    CommitInfo { change_id  :: !Text
               , subject    :: !Text
               , created    :: !UTCTime
               , updated    :: !UTCTime
               , insertions :: !Int
               , deletions  :: !Int }
    deriving Show

-- | Specific info about a file in a commit.
data FileInfo =
    FileInfo { filePath      :: !Text
             , linesDeleted  :: !Int
             , linesInserted :: !Int }
    deriving Show
