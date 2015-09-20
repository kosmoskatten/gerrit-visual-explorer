{-# LANGUAGE RecordWildCards #-}
module Gerrit.Store.Insert
    ( insertCommitEntries
    ) where

import Control.Concurrent.STM (STM, atomically, modifyTVar')
import Control.Monad (foldM)
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import Gerrit.Source.Types
import Gerrit.Store.Types
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Vector as Vector

type TempFileMap = HashMap Text [CommitEntry]

insertCommitEntries :: [GerritCommitEntry] -> CommitData -> IO CommitData
insertCommitEntries xs CommitData {..} = do
    let (commitSet', prepEntries) = foldl' prepareCommitEntry 
                                           (commitSet, []) xs
        files                     = concatMap extractFileInfo xs
    fileMap' <- atomically (foldM updateFileMap fileMap files)
    let (_, tfm, compEntries)     = foldl' completeCommitEntry
                                           (fileMap', HashMap.empty, [])
                                           prepEntries
        compEntries'              = Vector.fromList compEntries
    atomically $ mapM_ (updateFileTimeLine fileMap') (HashMap.toList tfm)
    return CommitData { timeLine  = Vector.concat [timeLine, compEntries']
                      , fileMap   = fileMap'
                      , commitSet = commitSet' } 

-- | Prepare a CommitEntry. To be prepared, check if the commit already 
-- is present in the CommitSet. If so, no further actions are made.
-- If not present, a CommitEntry with an empty changes vector is prepared,
-- and the CommitSet is updated accordingly.
prepareCommitEntry :: (CommitSet, [(CommitEntry, GerritCommitEntry)]) 
                   -> GerritCommitEntry 
                   -> (CommitSet, [(CommitEntry, GerritCommitEntry)])
prepareCommitEntry acc@(cs, xs) gce@(GerritCommitInfo {..}, _)
    | not (HashSet.member changeId cs) =
        let commitEntry = CommitEntry { timeStamp = updated
                                      , commitId  = changeId
                                      , subject   = commitSubject
                                      , changes   = Vector.empty
                                      }
            cs'         = HashSet.insert changeId cs
        in (cs', (commitEntry, gce):xs) 
    | otherwise                        = acc

-- | Update the FileMap. If a file not is present in the map, insert
-- an "empty" file.
updateFileMap :: FileMap -> GerritFileInfo -> STM FileMap
updateFileMap fm GerritFileInfo {..}
    | not (HashMap.member filePath fm) = do
        newFile <- emptyFile filePath
        return (HashMap.insert filePath newFile fm)
    | otherwise                        = return fm

completeCommitEntry :: (FileMap, TempFileMap, [CommitEntry])
                    -> (CommitEntry, GerritCommitEntry)
                    -> (FileMap, TempFileMap, [CommitEntry])
completeCommitEntry (fm, tfm, xs) (ce, (_, fileInfos)) =
    let changes' = map (prepareChange fm) fileInfos
        ce'      = ce { changes = Vector.fromList changes' }
        files    = map extractFile fileInfos
        tfm'     = foldl' (updateFileWithCommit ce') tfm files
    in (fm, tfm, ce':xs)

prepareChange :: FileMap -> GerritFileInfo -> Change
prepareChange fileMap GerritFileInfo {..} =
    -- In the updated file map be can assume that the requested file
    -- always is present.
    Change { insertions = linesInserted
           , deletions  = linesDeleted
           , file       = fromJust (HashMap.lookup filePath fileMap) }

updateFileWithCommit :: CommitEntry -> TempFileMap -> Text -> TempFileMap
updateFileWithCommit ce tfm name =
    HashMap.insertWith (\[new] old -> new:old) name [ce] tfm  

updateFileTimeLine :: FileMap -> (Text, [CommitEntry]) -> STM ()
updateFileTimeLine fm (name, xs) = do
    let ceVec = Vector.fromList xs
        file  = fromJust (HashMap.lookup name fm)
    modifyTVar' (partOf file) (\oldVec -> Vector.concat [oldVec, ceVec])

extractFileInfo :: GerritCommitEntry -> [GerritFileInfo]
extractFileInfo (_, xs) = xs

extractFile :: GerritFileInfo -> Text
extractFile GerritFileInfo {..} = filePath
