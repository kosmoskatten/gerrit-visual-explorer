{-# LANGUAGE RecordWildCards #-}
module Gerrit.Store.Import
    ( prependCommits
    ) where

import Control.Monad (foldM)
import Data.Hashable (hash)
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import Gerrit.Fetch.Types
import Gerrit.Store.Types
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set
import qualified Data.Vector as V

-- | A temporary file map that will serve as a scratchpad until updating
-- the real FileMap. The TempFileMap works on lists instead of Vectors,
-- and is more efficient to incrementally build up.
type TempFileMap = IntMap (Text, [CommitEntry])

-- | Import and prepent gerrit commits into the store. The timeline is
-- ordered with the latest commits first.
prependCommits :: [GerritCommitEntry] -> CommitStore -> CommitStore
prependCommits [] store            = store
prependCommits es CommitStore {..} =
    -- First. Make sure to filter commits that already are present, and
    -- update the commit set with the new commits.
    let (commitSet', es') = foldl' syncWithCommits (commitSet, []) es

        -- Next process each gerrit entry to get commit entries instead.
        (tfm', ces)       = foldl' processEntry (IM.empty, []) es'

        -- Now update the FileMap where each file has its timeline of
        -- commits.
        fileMap'          = foldl' updateFileMap fileMap (IM.toList tfm')

    -- Finally, create the new CommitStore.
    in CommitStore 
       { timeLine  = V.concat [ V.fromList ces, timeLine ]
       , fileMap   = fileMap'
       , commitSet = commitSet'
       }

-- | Sync a gerrit commit entry with the commit set. If the entry already
-- is present it will be discarded. If not present it will be kept and
-- the commit id will be stored in the commit set.
syncWithCommits :: (CommitSet, [GerritCommitEntry])
                -> GerritCommitEntry
                -> (CommitSet, [GerritCommitEntry])
syncWithCommits acc@(cs, xs) entry@(GerritCommitInfo {..}, _)
    | not (Set.member changeId cs) = (Set.insert changeId cs, entry:xs)
    | otherwise                   = acc

-- | Process a gerrit entry into a CommitEntry.
processEntry :: (TempFileMap, [CommitEntry])
             -> GerritCommitEntry
             -> (TempFileMap, [CommitEntry])
processEntry (tfm, xs) (GerritCommitInfo {..}, files) =
    let changeList  = map mkChange files
        commitEntry = CommitEntry { timeStamp = updated
                                  , commitId  = changeId
                                  , subject   = commitSubject
                                  , changes   = V.fromList changeList
                                  }
        filePaths   = map filePath files
        tfm'        = foldl' (annotateFilePath commitEntry) tfm filePaths
    in (tfm', commitEntry:xs)

-- | Make a Change record from the content of a gerrit file info.
mkChange :: GerritFileInfo -> Change
mkChange GerritFileInfo {..} =
    Change { insertions = linesInserted
           , deletions  = linesDeleted
           , fileRef    = hash filePath
           }

-- | Annotate a file path in the TempFileMap with a CommitEntry.
annotateFilePath :: CommitEntry -> TempFileMap -> Text -> TempFileMap
annotateFilePath entry tfm name = 
    IM.insertWith insertValue (hash name) (name, [entry]) tfm 
        where insertValue (_, [new]) (n, xs) = (n, new:xs)

-- | Update an entry in the FileMap with contents taken from the
-- temporary file map.
updateFileMap :: FileMap -> (FileHash, (Text, [CommitEntry])) -> FileMap
updateFileMap fm (key, (name', xs)) =
    IM.insertWith insertValue key 
                       File { name   = name'
                            , partOf = V.fromList xs 
                            } fm 
        where insertValue new old =
                 old { partOf = V.concat [ partOf new, partOf old ] }

