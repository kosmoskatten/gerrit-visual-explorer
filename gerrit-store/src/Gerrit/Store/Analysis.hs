{-# LANGUAGE RecordWildCards #-}
module Gerrit.Store.Analysis
    ( summaryPerActiveDay
    ) where

import Data.Map.Strict (Map)
import Data.Time (UTCTime (..), Day)
import Data.Vector (Vector)
import Gerrit.Store.Types
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

data Summary = 
    Summary { sumDay          :: !Day 
            , sumCommits      :: {-# UNPACK #-} !Int
            , sumChangedFiles :: {-# UNPACK #-} !Int
            , sumLinesIns     :: {-# UNPACK #-} !Int
            , sumLinesDel     :: {-# UNPACK #-} !Int
            }
    deriving Show

type SummaryMap = Map Day Summary

-- | Generate a vector of activity summaries per active day. The vector is
-- sorted in cronological order.
summaryPerActiveDay :: CommitStore -> Vector Summary
summaryPerActiveDay = V.fromList . map snd . Map.toAscList . 
                        V.foldl' sumActivities Map.empty . timeLine

sumActivities :: SummaryMap -> CommitEntry -> SummaryMap
sumActivities sMap ce =
    let (numCh, numIns, numDel) = changeSummary ce
        day                     = utctDay (timeStamp ce)
        summary                 = Summary
                                  { sumDay          = day
                                  , sumCommits      = 1
                                  , sumChangedFiles = numCh
                                  , sumLinesIns     = numIns
                                  , sumLinesDel     = numDel
                                  }
    in Map.insertWith combineChanges day summary sMap
      where
        changeSummary :: CommitEntry -> (Int, Int, Int)
        changeSummary = V.foldl' (\(n, i, d) ch -> 
          (n + 1, i + insertions ch, d + deletions ch)) (0, 0, 0) . changes

        combineChanges :: Summary -> Summary -> Summary
        combineChanges new old =
            old { sumCommits      = sumCommits old + sumCommits new
                , sumChangedFiles = sumChangedFiles old + sumChangedFiles new
                , sumLinesIns     = sumLinesIns old + sumLinesIns new
                , sumLinesDel     = sumLinesDel old + sumLinesDel new
                }

