{-# LANGUAGE OverloadedStrings #-}
-- | Module which declare "raw" data fetched directly from a Gerrit
-- server.
module Gerrit.Source.Types
    ( GerritCommitInfo (..)
    , GerritFileInfo (..)
    , GerritCommitFilter
    , GerritCommitEntry
    ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)

import qualified Data.HashMap.Strict as HM

-- | General commit info for a Gerrit commit.
data GerritCommitInfo = 
    GerritCommitInfo { changeId      :: !Text
                     -- ^ The selected commit id which is change_id 
                     -- from Gerrit.
                     , commitSubject :: !Text
                     -- ^ The commit's subject.
                     , updated       :: !UTCTime
                     }
    deriving Show

data GerritChangeMap =
    GerritChangeMap { allChanges :: ![GerritFileInfo] }
    deriving Show

-- | Specific info about a file in a Gerrit commit.
data GerritFileInfo =
    GerritFileInfo { filePath      :: !Text
                   -- ^ Full path of the commited file.
                   , linesDeleted  :: {-# UNPACK #-} !Int
                   -- ^ Lines deleted.
                   , linesInserted :: {-# UNPACK #-} !Int
                   -- ^ Lines inserted.
                   }
    deriving Show

-- | A filter function which can select a subset of the commits.
type GerritCommitFilter = ([GerritCommitInfo] -> IO [GerritCommitInfo])

-- | A commit entry with the commit info and a list of files.
type GerritCommitEntry = (GerritCommitInfo, [GerritFileInfo])

-- | FromJSON instance for GerritCommitInfo.
instance FromJSON GerritCommitInfo where
    parseJSON (Object o) =
        GerritCommitInfo <$> o .: "change_id"
                         <*> o .: "subject"
                         <*> (read <$> o .: "updated")
    parseJSON _          = mzero

-- | FromJSON instance for GerritChangeMap.
instance FromJSON GerritChangeMap where
    parseJSON (Object v) = do
        -- Parsing the change map is a little bit messy. First, convert
        -- the hash map to a list of file infos. The complete map entry
        -- must be included in order to create the file info.  
        xs <- mapM parseFileInfo (HM.toList v)
        return GerritChangeMap { allChanges = xs }
        where 
            parseFileInfo (name, Object o) = do
                -- The field lines_deleted/lines_inserted may or
                -- may not be present in the record. If they not are
                -- present, interpret the resulting value as zero.
                deletions  <- if HM.member "lines_deleted" o then
                                  o .: "lines_deleted"
                              else return 0
                insertions <- if HM.member "lines_inserted" o then
                                  o .: "lines_inserted"
                              else return 0
                return GerritFileInfo { filePath      = name
                                      , linesDeleted  = deletions
                                      , linesInserted = insertions
                                      }
            parseFileInfo _                = mzero            

    parseJSON _          = mzero

